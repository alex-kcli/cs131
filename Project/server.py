# https://cs131portserver-314508.wl.r.appspot.com/?uid=205366842
# Your ports are: 12250 through 12254. Please do not use other ports

import asyncio
import argparse
import sys
import re
import time
import aiohttp
import json

# port number dict
ports = {
     "Riley": 12250, 
     "Jaquez": 12251,
     "Juzang": 12252, 
     "Campbell": 12253, 
     "Bernard": 12254
}


# communication dict
communicates = {
    "Riley": ["Jaquez", "Juzang"], 
    "Jaquez": ["Riley", "Bernard"],
    "Juzang": ["Campbell", "Riley", "Bernard"], 
    "Campbell": ["Juzang", "Bernard"], 
    "Bernard": ["Jaquez", "Juzang", "Campbell"]
}


def start_log_file(server_name):
    log_file = open(server_name + ".txt", "w")
    return log_file


def log_update(m_content):
    log_file.write(m_content + "\n")
    
    
def get_time_difference(time1, time2):
    if time1 <= time2:
        diff = time2 - time1
        return "+" + str(diff)
    else:
        diff = time1 - time2
        return "-" + str(diff)


def is_floatint(num):
    return num.replace('.','',1).isdigit()
    

def check_valid_coord(coordinate):
    coords_list = re.split('[+ -]', coordinate)
    if len(coords_list) != 3:
        return False
    return (not coords_list[0]) and is_floatint(coords_list[1]) and is_floatint(coords_list[2])
    

def check_valid_time(timestamp):
    return is_floatint(timestamp)


def get_lat_lon(coordinate):
    i = 1
    while i < len(coordinate):
        if (not coordinate[i].isdigit()) and (not coordinate[i] == "."):
            lat = coordinate[:i]
            lon = coordinate[i:]
            break
        else:
            i += 1
    return lat, lon
    


class Server:
    def __init__(self, name, port):
        self.name = name
        self.ip = '127.0.0.1'
        self.port = port
        self.message_max_length = 10000
        self.clients_dict = dict()
        
        
    async def run_forever(self):
        server = await asyncio.start_server(self.handle_message, self.ip, self.port)
        log_update(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
        server.close()
    
    
    async def handle_message(self, reader, writer):
        data = await reader.read(self.message_max_length)
        message = data.decode()
        addr = writer.get_extra_info('peername')
        log_update("{} received {} from {}".format(self.name, message, addr))
        
        
        msg_list = [msg for msg in message.strip().split() if msg]
        
        res = str()
        if not msg_list:
            res = "? " + message
            log_update("Responded to invalid message with ? " + message)
        elif not(((msg_list[0] == "IAMAT" or msg_list[0] == "WHATSAT") and len(msg_list) == 4) or (msg_list[0] == "AT" and len(msg_list) == 6)):
            res = "? " + message
            log_update("Responded to invalid message with ? " + message)
            
        else:
            if (msg_list[0] == "IAMAT"):
                if check_valid_coord(msg_list[2]) and check_valid_time(msg_list[3]):
                    res = await self.handle_i_am_at(*msg_list[1:])
                    log_update(f'Responded to IAMAT with {res}')
                else:
                    res = "? " + message
                    log_update("Responded to IAMAT with ? " + message)
                
            elif (msg_list[0] == "WHATSAT"):
                if int(msg_list[2]) > 50 or int(msg_list[2]) < 0:
                    res = "? " + message
                    log_update("Responded to WHATSAT with ? " + message)
                elif int(msg_list[3]) > 20 or int(msg_list[3]) < 0:
                    res = "? " + message
                    log_update("Responded to WHATSAT with ? " + message)
                else:
                    res = await self.handle_whats_at(*msg_list[1:])
                    log_update(f'Responded to WHATSAT with {res}')
            
            else: # case "AT"
                await self.handle_at(*msg_list[1:])
                
        writer.write(res.encode())
        await writer.drain()
        
        log_update("Close the client socket")
        writer.close()
        
        
        
    async def handle_i_am_at(self, client_id, coordinates, timestamp):
        curr_time = time.time()
        time_diff_str = get_time_difference(float(timestamp), float(curr_time))
        
        res = f'AT {self.name} {time_diff_str} {client_id} {coordinates} {timestamp}'     
        self.clients_dict[client_id] = [res, coordinates, timestamp]
        await self.flooding(res)
        return res

    
    
    async def handle_whats_at(self, client_id, radius, max_results):
        msg = self.clients_dict[client_id][0]
        coord = self.clients_dict[client_id][1]
        lat, lon = get_lat_lon(coord)
        radius_mt = 1000 * int(radius)
        site = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=" + lat + "," + lon + "&radius=" + str(radius_mt) + "&key=" + api_key
#        site = "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location=+34.068930,-118.445127&radius=" + str(radius_mt) + "&key=" + api_key
        async with aiohttp.ClientSession(connector=aiohttp.TCPConnector(ssl=False)) as session: 
            async with session.get(site) as response: 
                res = await response.json()
        if len(res['results']) > int(max_results):
            res['results'] = res['results'][:int(max_results)]
        ans_raw = str(json.dumps(res, indent=4))
        ans_eli_cons = re.sub('\n+', '\n', ans_raw)
        ans = ans_eli_cons.rstrip('\n')
        ret = msg + "\n" + ans + "\n\n"
        return ret
    
    
    
    async def handle_at(self, server_id, time_diff, client_id, coordinates, timestamp):
        if (client_id in self.clients_dict) and (float(timestamp) <= float(self.clients_dict[client_id][2])):
            return
        else:
            res = f'AT {server_id} {time_diff} {client_id} {coordinates} {timestamp}'     
            self.clients_dict[client_id] = [res, coordinates, timestamp]
            await self.flooding(res)
            
                
        
    async def flooding(self, message):
         for server_name in communicates[self.name]:
             try:
                 reader, writer = await asyncio.open_connection('127.0.0.1', ports[server_name])
                 log_update(f'Connected to {server_name}') 
                 writer.write(message.encode())
                 log_update(f'{self.name} sent {message} to {server_name}') 
                 
                 await writer.drain()
                 writer.close()
                 await writer.wait_closed()
                 log_update(f'Closed connection with {server_name}') 
                 
             except:
                 log_update(f'Failed connection with {server_name}')
        
        

def main():
    server = Server(server_name, ports[server_name])
    try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass
    log_file.close()



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()
    server_name = args.server_name
    
    if server_name not in ports:
        print(f'Invalid server name {args.server_name}, exit with code 1')
        sys.exit(1)
    
    log_file = start_log_file(server_name);
    
    main()
    

    

