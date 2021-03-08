package zio.sql

import zio.Schedule
import zio.duration._

/**
 * Configuration information for the connection pool.
 *
 * @param url            The JDBC connection string.
 * @param properties     JDBC connection properties (username / password could go here).
 * @param poolSize       The size of the pool.
 * @param queueCapacity  The capacity of the queue for connections. When this size is reached, back pressure will block attempts to add more.
 * @param retryPolicy    The retry policy to use when acquiring connections.
 */
final case class ConnectionPoolConfig(
  url: String,
  properties: java.util.Properties,
  poolSize: Int = 10,
  queueCapacity: Int = 1000,
  retryPolicy: Schedule[Any, Exception, Any] = Schedule.recurs(20) && Schedule.exponential(10.millis)
)
