#!/usr/bin/env python3
import pika

connection = pika.BlockingConnection(
    pika.ConnectionParameters(host='localhost',
                              credentials=pika.PlainCredentials('chad', 'chad')))
channel = connection.channel()

channel.queue_declare(queue='gigachad.weatherapp')

channel.basic_publish(exchange='',
                      routing_key='hello',
                      body='Hello World!')
print(" [x] Sent 'Hello World!'")

connection.close()
