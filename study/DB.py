#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import pymysql.cursors

class MySQL(object):
  def __init__(self, host, port, user, pw, db):
    self.__result = None
    self.__lastrowid = None

    self.__host = host
    self.__port = port
    self.__user = user
    self.__pass = pw
    self.__db   = db

  def __enter__(self):
    self.__conn = pymysql.connect \
    (
      host=self.__host,
      port=self.__port,
      user=self.__user,
      password=self.__pass,
      db=self.__db,
      charset='utf8mb4',
      cursorclass=pymysql.cursors.SSDictCursor,
      autocommit=False
    )
    return self

  def __exit__(self, type, value, tb):
    self.__conn.close()
    del(self.__conn)

  def __del__(self):
    del(self.__lastrowid)
    del(self.__result)


  # Public methods.
  def exec_query(self, query, params=[None]):
    with self.__conn.cursor() as c:
      ret = c.executemany(query, params)
      self.__lastrowid = c.lastrowid
      self.__result = c.fetchall()
      return ret

  def get_result(self):
    # For disabling query cache with Python binding.
    self.commit()
    return self.__result

  def get_last_rowid(self):
    return self.__lastrowid

  def begin(self):
    self.__conn.begin()

  def commit(self):
    self.__conn.commit()
