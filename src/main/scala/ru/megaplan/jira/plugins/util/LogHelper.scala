package ru.megaplan.jira.plugins.util

import org.apache.log4j.Logger

trait LogHelper {
  private val loggerName = this.getClass.getName
  lazy val log = Logger.getLogger(loggerName)
}