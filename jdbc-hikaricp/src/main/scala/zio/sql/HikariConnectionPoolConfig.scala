package zio.sql

import com.zaxxer.hikari.HikariConfig

/**
 * Configuration information for the connection pool.
 *
 * @param url            The JDBC connection string.
 * @param properties     JDBC connection properties (username / password could go here).
 * @param poolSize       The size of the pool.
 * @param connectionTimeout  Maximum number of milliseconds that a client will wait for a connection from the pool.
 *                           If this time is exceeded without a connection becoming available, a SQLException will be thrown from javax.sql.DataSource.getConnection().
 * @param idleTimeout   This property controls the maximum amount of time (in milliseconds) that a connection is allowed to sit idle in the pool.
 *                      Whether a connection is retired as idle or not is subject to a maximum variation of +30 seconds, and average variation of +15 seconds.
 *                      A connection will never be retired as idle before this timeout. A value of 0 means that idle connections are never removed from the pool.
 * @param initializationFailTimeout the number of milliseconds before the
 *        pool initialization fails, or 0 to validate connection setup but continue with
 *        pool start, or less than zero to skip all initialization checks and start the
 *        pool without delay.
 * @param maxLifetime This property controls the maximum lifetime of a connection in the pool.
 *                    When a connection reaches this timeout, even if recently used, it will be retired from the pool.
 *                    An in-use connection will never be retired, only when it is idle will it be removed. Should be bigger then 30000
 * @param minimumIdle The property controls the minimum number of idle connections that HikariCP tries to maintain in the pool, including both idle and in-use connections.
 *                    If the idle connections dip below this value, HikariCP will make a best effort to restore them quickly and efficiently.
 * @param connectionInitSql the SQL to execute on new connections
 *                    Set the SQL string that will be executed on all new connections when they are
 *                    created, before they are added to the pool.  If this query fails, it will be
 *                    treated as a failed connection attempt.
 */
final case class HikariConnectionPoolConfig(
  url: String,
  userName: String,
  password: String,
  poolSize: Int = 10,
  autoCommit: Boolean = true,
  connectionTimeout: Option[Long] = None,
  idleTimeout: Option[Long] = None,
  initializationFailTimeout: Option[Long] = None,
  maxLifetime: Option[Long] = None,
  minimumIdle: Option[Int] = None,
  connectionInitSql: Option[String] = None
) {
  private[sql] def toHikariConfig = {
    val hikariConfig = new HikariConfig()
    hikariConfig.setJdbcUrl(this.url)
    hikariConfig.setAutoCommit(this.autoCommit)
    hikariConfig.setMaximumPoolSize(this.poolSize)
    hikariConfig.setUsername(userName)
    hikariConfig.setPassword(password)
    connectionTimeout.foreach(hikariConfig.setConnectionTimeout)
    idleTimeout.foreach(hikariConfig.setIdleTimeout)
    initializationFailTimeout.foreach(hikariConfig.setInitializationFailTimeout)
    maxLifetime.foreach(hikariConfig.setMaxLifetime)
    minimumIdle.foreach(hikariConfig.setMinimumIdle)
    connectionInitSql.foreach(hikariConfig.setConnectionInitSql)
    hikariConfig
  }
}
