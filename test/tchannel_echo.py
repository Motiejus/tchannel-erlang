#!/usr/bin/env python

import sys

import tornado
from tchannel import TChannel

tchannel = TChannel('echo-server', hostport='127.0.0.1:0')

def close_callback(_data):
    stdin.close()
    tornado.ioloop.IOLoop.current().stop()
    sys.exit(1)

@tchannel.json.register('/echo')
def handler(request):
    return request.body

@tchannel.json.register('/exit')
def handler(request):
    tornado.ioloop.IOLoop.current().close()
    sys.exit()

if __name__ == '__main__':
    tchannel.listen()
    stdin = tornado.iostream.PipeIOStream(sys.stdin.fileno())
    stdin.read_until_close(callback=close_callback)
    print(tchannel.hostport)
    sys.stdout.flush()
    tornado.ioloop.IOLoop.current().start()
