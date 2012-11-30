package ru.megaplan.jira.plugins.megareport.businessprojectsspentreport

import com.atlassian.jira.plugin.report.impl.AbstractReport
import com.atlassian.jira.web.action.ProjectActionSupport
import java.lang.String

import org.ofbiz.core.entity.jdbc.SQLProcessor
import annotation.tailrec
import com.atlassian.jira.security.groups.GroupManager
import java.util.Date
import scalaz._
import Scalaz._
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import collection.immutable.TreeMap
import collection.mutable
import ru.megaplan.jira.plugins.util.LogHelper
import scala.Predef._
import com.atlassian.core.util.DateUtils
import java.text.DecimalFormat

import collection.JavaConversions._
import com.atlassian.jira.util.{I18nHelper, ParameterUtils}
import com.atlassian.jira.security.JiraAuthenticationContext
import java.sql.ResultSet
import java.security.Timestamp

class BusinessProjectsSpentReport
(groupManager: GroupManager,
 authenticationContext: JiraAuthenticationContext)
  extends AbstractReport with LogHelper {
  private val ENTITY_DS: String = "defaultDS"
  private val TEAMGROUPPREFIX = "report-team-"
  private val mainStatement: String = """
                                        |with businessprojects1 as (
                                        |	select customvalue, issue from customfieldvalue cfv
                                        |	join customfieldoption cfo on cfo.id = cast(cfv.stringvalue as numeric)
                                        |	join customfield cf on cf.id = cfv.customfield and cf.cfname = 'Цель'
                                        |	order by issue
                                        |), businessprojects as (
                                        |	select customvalue, issue, (select count(*) from businessprojects1 where issue = bp1.issue) as ct from businessprojects1 bp1
                                        |), spents as (
                                        |	select customvalue, author, date_trunc('month', startdate) as mont, timeworked/ct as timeworked from businessprojects bp
                                        |	join worklog wl on wl.issueid = bp.issue
                                        | where startdate > ? and startdate < ?
                                        |), spent as (
                                        |	select mont::timestamp without time zone, customvalue, author, cu.display_name as username, sum(timeworked) as timeworked, g.group_name as gp from spents
                                        |	join cwd_membership cwm on cwm.child_name = author
                                        |	join cwd_group g on g.group_name = cwm.parent_name
                                        |	join cwd_user cu on cu.user_name = author
                                        |	where g.group_name like ? || '%'
                                        |	group by mont, customvalue, author, cu.display_name, g.group_name
                                        |	order by mont, customvalue, author
                                        |)
                                        |select * from spent
                                      """.stripMargin


  override def generateReportExcel(projectActionSupport: ProjectActionSupport, map: java.util.Map[_, _]) =
    generateReport(projectActionSupport, map, true)

  override def generateReportHtml(projectActionSupport: ProjectActionSupport, map: java.util.Map[_, _]) =
    generateReport(projectActionSupport, map, false)

  override def isExcelViewSupported = true

  def generateReport(projectActionSupport: ProjectActionSupport, params: java.util.Map[_, _], isExcel: Boolean): String = {
    type UserName = String
    type DisplayName = String
    type GroupName = String
    type SpentSeconds = Long
    type ProjName = String
    type Percents = Float

    type SpentRow = (ProjName, UserName, SpentSeconds)
    type SpentTable = List[SpentRow]
    type Spents = SpentTable


    type GroupsResults = Map[UserName, GroupName]
    type GroupsErrors = Map[UserName, List[GroupName]]



    def groups(users: Set[UserName]): (GroupsResults, GroupsErrors) = {
      def reportGroups(user: String): Iterable[String] = {
        groupManager.getGroupNamesForUser(user).filter(_.startsWith(TEAMGROUPPREFIX)).map(_.drop(TEAMGROUPPREFIX.length))
      }
      users.foldLeft(Map.empty[UserName, GroupName], Map.empty[UserName, List[GroupName]]) {
        (maps, name) => {
          val gs: List[GroupName] = reportGroups(name).toList
          if (gs.size == 1) {
            (maps._1 + (name -> gs.head), maps._2)
          } else {
            (maps._1, maps._2 + (name -> gs))
          }
        }
      }
    }




    val (mspents, groupsResults, groupsErrors, displayNames):
    (Map[Date, Spents], GroupsResults, GroupsErrors, Map[UserName, DisplayName]) = {
      val rs: ResultSet = {
        val sqlProcessor: SQLProcessor = new SQLProcessor(ENTITY_DS)
        sqlProcessor.prepareStatement(mainStatement)
        def getDate(n: String, default: Date) = {
          import Option.{apply => ?}
          ?(ParameterUtils.getDateParam(params, n, authenticationContext.getLocale)).getOrElse(default)
        }

        val Array(start, end) = Array(getDate("startDate", new Date(0)),getDate("endDate", new Date())).sorted
        List(start, end, TEAMGROUPPREFIX) foreach {
          case v: String => sqlProcessor.setValue(v)
          case v: Date => sqlProcessor.setValue(new java.sql.Timestamp(v.getTime))
        }
        sqlProcessor.executeQuery
      }
      @tailrec
      def loop(r: Map[Date, Spents], users: Map[UserName, DisplayName]): (Map[Date, Spents], Map[UserName, DisplayName]) = {
        if (!rs.next()) (r.mapValues(_.reverse), users)
        else {
          val (time, proj, userName, displayName, spent) = (
              rs.getDate(1),
              rs.getString(2),
              rs.getString(3),
              rs.getString(4),
              rs.getLong(5)
          )
          val curSpents = r.getOrElse(time, List.empty)
          val newSpents = (proj, userName, spent) :: curSpents
          loop(r.updated(time, newSpents), users.updated(userName, displayName))
        }
      }
      val l = loop(Map.empty, Map.empty)
      val grs = groups(l._2.keySet)
      (l._1, grs._1, grs._2, l._2)
    }

    val (allSpents, spentsByMonth) = {
      val spentsByMonth:
      Map[Date, (Map[ProjName, SpentSeconds], Map[UserName, SpentSeconds], Map[(UserName, ProjName), SpentSeconds])]
      = mspents.mapValues(
        _.foldLeft(
          Map.empty[ProjName, SpentSeconds],
          Map.empty[UserName, SpentSeconds],
          Map.empty[(UserName, ProjName), SpentSeconds]
        ) {
          (maps, sr) => {
            var projName = sr._1
            val userName = sr._2
            val spent = sr._3
            (
              {
                val m = maps._1
                val v = m.getOrElse(projName, 0L) + spent
                m.updated(projName, v)
              },
              {
                val m = maps._2
                val v = m.getOrElse(userName, 0L) + spent
                m.updated(userName, v)
              },
              {
                val m = maps._3
                val k = (userName, projName)
                val v = m.getOrElse(k, 0L) + spent
                m.updated(k, v)
              }
            )
          }
        }
      )
      val allSpents = spentsByMonth.foldLeft(
        Map.empty[ProjName, SpentSeconds],
        Map.empty[UserName, SpentSeconds],
        Map.empty[(UserName, ProjName), SpentSeconds]) {
        case (red, cur) => {
          val (pspents, uspents, puspents) = red
          val (npspents, nuspents, npuspents) = cur._2
          (pspents |+| npspents, uspents |+| nuspents, puspents |+| npuspents)
        }
      }
      val (allpspents, alluspents, allpuspents) = allSpents
      val emptyAllSpents = {
        (
          allpspents.mapValues(_=>0L),
          alluspents.mapValues(_=>0L),
          allpuspents.mapValues(_=>0L)
        )
      }
      (allSpents, {
        spentsByMonth.foldLeft(spentsByMonth.empty) {
          (res, cur) => {
            val (curDate, curVals) = cur
            res + (curDate->(curVals |+| emptyAllSpents))
          }
        }
      })
    }
    val fmt = DateTimeFormat.forPattern("MMMM, yyyy")
    type TimeName = String
    def toOrderedDates[T](m: Map[Date, T]): mutable.LinkedHashMap[String, T] = {
      val sorted = mutable.LinkedHashMap(
        TreeMap(m.toArray:_*).toArray:_*
      )
      val result = sorted.map {
        case (k, v) => {
          val dt = new DateTime(k)
          (fmt.print(dt), v)
        }
      }
      result
    }
    val (projectsTableSpents, usersTable) = {
      val antiDate = new Date(0)
      val all = spentsByMonth + (antiDate->allSpents)
      val unsortedTables = all.mapValues {
        case (pspents, uspents, puspents) => {
          (
            pspents,
            puspents.foldLeft(Map.empty[ProjName, Map[GroupName, Map[UserName, (SpentSeconds, Percents)]]]) {
              case (res, ((uname, projName), spent)) => {
                val group = groupsResults(uname)
                val projStats = res.getOrElse(projName, Map.empty[GroupName, Map[UserName, (SpentSeconds, Percents)]])
                val groupStats = projStats.getOrElse(group, Map.empty[UserName, (SpentSeconds, Percents)])
                val newGroupStats = groupStats + (uname->(spent, {
                  val allSpent = uspents(uname)
                  if (allSpent == 0) {
                    0f
                  } else {
                    100f*spent/allSpent
                  }
                }))
                res.updated(projName, projStats.updated(group, newGroupStats))
              }
            }
            )
        }
      }.foldLeft(
        Map.empty[Date, Map[ProjName, SpentSeconds]]->
        Map.empty[Date, Map[ProjName, Map[GroupName, Map[UserName, (SpentSeconds, Percents)]]]]
      ) {
        case ((p, u), (date, (projects, users))) => {
          val np = p + (date->projects)
          val nu = u + (date->users)
          (np, nu)
        }
      }
      val ordProjects = toOrderedDates(unsortedTables._1)
      val ordUsers = toOrderedDates(unsortedTables._2)
      val resProjects = ordProjects.tail + ("Sum" -> ordProjects.head._2)
      val resUsers = ordUsers.tail + ("Sum" -> ordUsers.head._2)
      log.warn("resUsers : " + resUsers.keySet)
      (resProjects, resUsers)
    }

    log.warn("usersTable : " + usersTable.keySet)

    import scala.collection.JavaConverters._
    def prettyDuration(l: Long) = DateUtils.getDurationString(l)
    def percentify(l: Long, sum: Long): Float = {
      if (sum == 0) 0f
      else
        100f*l/sum
    }

    val monthSumTable = {
      val mst = projectsTableSpents.mapValues(_.foldLeft(0L)(_+_._2))
      mst.mapValues(spent => {
        prettyDuration(spent)
      })
    }

    val projectsTable = projectsTableSpents.mapValues(monthSpents => {
      val monthSum = monthSpents.foldLeft(0L)(_+_._2)
      monthSpents.mapValues(spent => {
        (spent, percentify(spent, monthSum))
      })
    })

    val projectsSumAllMonthsTable = {
      val (sums, megasum) = projectsTableSpents.foldLeft(Map.empty[ProjName, SpentSeconds], 0L) {
        case ((res, supersum), (monthName, projData)) => {
          (res |+| projData, supersum + projData.foldLeft(0L)(_+_._2))
        }
      }
      sums.mapValues(ss => {
        (ss, percentify(ss, megasum))
      })
    }

    val groupsTable = usersTable.mapValues(projectsData =>
      projectsData.mapValues(groupsData => {
        //sum in this month in this project for all groups
        val (groupsSum, groupsSums) = groupsData.foldLeft(0L, Map.empty[GroupName, SpentSeconds]) {
          case ((sum, sums), (groupName, usersData)) => {
            val spentSum = usersData.foldLeft(0L)(_+_._2._1)
            (sum + spentSum, sums.updated(groupName, spentSum))
          }
        }
        groupsData.map {
          case (groupName, usersStats) => {
          val groupSum = groupsSums(groupName)
          val percents = percentify(groupSum, groupsSum)
          (groupName->(groupSum, percents))
        }}
      })
    )

    def javaifyMap[K1, V1, K2, V2](m: scala.collection.Map[K1, V1]): java.util.Map[K1, _] = {
      val formatter = new DecimalFormat("#.##")
      (m.mapValues {
        v => {
          v match {
            case vm: Map[K2, V2] => {
              javaifyMap(vm)
            }
            case tuple: Product => {
              val r = tuple.productIterator.foldLeft(Map.empty[Int, Any], 0) {
                case ((res, count), element) => {
                  val el = {
                    element match {
                      case e: Long => {
                        prettyDuration(e)
                      }
                      case e: Float => {
                        formatter.format(e)+'%'
                      }
                      case e => e
                    }
                  }
                  (res.updated(count, el), count + 1)

                }
              }._1
              javaifyMap(r)
            }
            case vm => vm
          }
        }
      }).asJava
    }

    val vp = (
      Map(
        "projectsTable" -> projectsTable,
        "monthSumTable" -> monthSumTable,
        "groupsTable" -> groupsTable,
        "usersTable" -> usersTable,
        "displayNames" -> displayNames,
        "projectsSumAllMonthsTable" -> projectsSumAllMonthsTable
      ).mapValues {
        v => javaifyMap(v)
      } ++ Map(
        "allProjects" -> projectsTable.last._2.keys.asJava
      )
    ).asJava



    descriptor.getHtml({if (!isExcel) "view" else "excel"}, vp)
  }

}

