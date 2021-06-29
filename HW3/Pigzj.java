import java.util.zip.*;
import java.io.*;
import java.nio.file.*;


class SingleThreadedGZipCompressor {
    public final static int BLOCK_SIZE = 131072; // 128kb
    public final static int DICT_SIZE = 32768; // 32kb
    private final static int GZIP_MAGIC = 0x8b1f;
    private final static int TRAILER_SIZE = 8;
    
    public InputStream inStream;
    public ByteArrayOutputStream outStream;
    private CRC32 crc = new CRC32();
    
    public SingleThreadedGZipCompressor(InputStream in_stream) {
        this.inStream = in_stream;
        this.outStream = new ByteArrayOutputStream();
    }
    
    
    private void writeHeader() throws IOException {
        outStream.write(new byte[] {
            (byte) GZIP_MAGIC,          // Magic number (short)
            (byte)(GZIP_MAGIC >> 8),    // Magic number (short)
            Deflater.DEFLATED,          // Compression method (CM)
            0,                          // Flags (FLG)
            0,                          // Modification time MTIME (int)
            0,                          // Modification time MTIME (int)
            0,                          // Modification time MTIME (int)
            0,                          // Modification time MTIME (int)Sfil
            0,                          // Extra flags (XFLG)
            0                           // Operating system (OS)
        });
    }
    
    
    /*
     * Writes GZIP member trailer to a byte array, starting at a given
     * offset.
     */
    private void writeTrailer(long totalBytes, byte[] buf, int offset) throws IOException {
        writeInt((int)crc.getValue(), buf, offset); // CRC-32 of uncompr. data
        writeInt((int)totalBytes, buf, offset + 4); // Number of uncompr. bytes
    }

    /*
     * Writes integer in Intel byte order to a byte array, starting at a
     * given offset.
     */
    private void writeInt(int i, byte[] buf, int offset) throws IOException {
        writeShort(i & 0xffff, buf, offset);
        writeShort((i >> 16) & 0xffff, buf, offset + 2);
    }

    /*
     * Writes short integer in Intel byte order to a byte array, starting
     * at a given offset
     */
    private void writeShort(int s, byte[] buf, int offset) throws IOException {
        buf[offset] = (byte)(s & 0xff);
        buf[offset + 1] = (byte)((s >> 8) & 0xff);
    }

    public void compress() throws FileNotFoundException, IOException {
        this.writeHeader();
        this.crc.reset();
        
        /* Buffers for input blocks, compressed bocks, and dictionaries */
        byte[] blockBuf = new byte[BLOCK_SIZE];
        byte[] cmpBlockBuf = new byte[BLOCK_SIZE * 2];
        byte[] dictBuf = new byte[DICT_SIZE];
        
        Deflater compressor = new Deflater(Deflater.DEFAULT_COMPRESSION, true);
        
//        File file = new File(this.fileName);

//        long fileBytes = file.length();
//        InputStream inStream = new FileInputStream(file);
        
        long totalBytesRead = 0;
        boolean hasDict = false;

        int nBytes = inStream.read(blockBuf);
        totalBytesRead += nBytes;

        while (nBytes > 0) {
            /* Update the CRC every time we read in a new block. */
            crc.update(blockBuf, 0, nBytes);
            compressor.reset();
            
            /* If we saved a dictionary from the last block, prime the deflater with it */
            if (hasDict) {
                compressor.setDictionary(dictBuf);
            }
            compressor.setInput(blockBuf, 0, nBytes);
            
            
            /* Otherwise, just deflate and then write the compressed block out. Not using SYNC_FLUSH here leads to
             * some issues, but using it probably results in less efficient compression. There's probably a better
             * way to deal with this. */
            int deflatedBytes = compressor.deflate(cmpBlockBuf, 0, cmpBlockBuf.length, Deflater.SYNC_FLUSH);
            if (deflatedBytes > 0) {
                outStream.write(cmpBlockBuf, 0, deflatedBytes);
            }
            
            /* If we read in enough bytes in this block, store the last part as the dictionary for the
             * next iteration */
            if (nBytes >= DICT_SIZE) {
                System.arraycopy(blockBuf, nBytes - DICT_SIZE, dictBuf, 0, DICT_SIZE);
                hasDict = true;
            } else {
                hasDict = false;
            }
            
            nBytes = inStream.read(blockBuf);
            totalBytesRead += nBytes;
        }
        
        /* If we've read all the bytes in the file, this is the last block.
         * We have to clean out the deflater properly */
        if (!compressor.finished()) {
            compressor.finish();
            while (!compressor.finished()) {
                int deflatedBytes = compressor.deflate(cmpBlockBuf, 0, cmpBlockBuf.length, Deflater.NO_FLUSH);
                if (deflatedBytes > 0) {
                    outStream.write(cmpBlockBuf, 0, deflatedBytes);
                }
            }
        }

        /* Finally, write the trailer and then write to STDOUT */
        byte[] trailerBuf = new byte[TRAILER_SIZE];
        if (nBytes == 0) {
            writeTrailer(totalBytesRead, trailerBuf, 0);
        }
        else {
            writeTrailer(totalBytesRead+1, trailerBuf, 0);
        }
        outStream.write(trailerBuf);
        outStream.writeTo(System.out);
    }
}


public class Pigzj {
    public static void main (String[] args) throws FileNotFoundException, IOException {
        int n_available = Runtime.getRuntime().availableProcessors();
        int n_processors = 1;
        
        for (int i = 0; i < args.length; i++) {
            if (args[i].equals("-p")) {
                if (i + 1 < args.length) {
                    int n_requested = Integer.parseInt(args[i+1]);
                    if (n_requested > n_available) {
                        System.err.println("Requesting more processors than the machine has available");
                        System.exit(1);
                    }
                    if (n_requested < 1) {
                        System.err.println("Invalid number of processors requested, must be at least 1");
                        System.exit(1);
                    }
                    n_processors = n_requested;
                }
            }
        }
        
        if (n_processors == 1) {
            SingleThreadedGZipCompressor s_cmp = new SingleThreadedGZipCompressor(System.in);
            s_cmp.compress();
        }
        else {      // n_processors > 1
            SingleThreadedGZipCompressor s_cmp = new SingleThreadedGZipCompressor(System.in);
            s_cmp.compress();
        }
    }
}
