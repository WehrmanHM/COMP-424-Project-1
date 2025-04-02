//import scala.collection.parallel.CollectionConverters.*
import scala.annotation.tailrec
import scala.util.Random

object PageRank {
    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return      A map of page.id to a weight of 1.0 for those same WebPage objects
     */
    def equal(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map { (key, _) => key -> 1.0 }
    }

    /**
     * @param pages A map of page.id to page for some number of WebPage objects
     * @return A map of page.id to a weight that is a simple count of the number of pages linking to that page
     */
    def indegree(pages: Map[String, WebPage]): Map[String, Double] = {
        pages.map { (key, _) => key ->
          pages.removed(key).map( { (subKey, page) => page.links.contains(key)}).size.toDouble
        }
    }

    def pagerank(pages: Map[String, WebPage]): Map[String, Double] = {
        val rnd = new Random()
        val steps = 100
        val users = 10000
        val ids = pages.keys


        def randomWalk(startId: String): Map[String, Int] = {
            @tailrec
            def walk(current: String, step: Int, counts: Map[String, Int]): Map[String, Int] = {
                if (step >= steps) then counts
                else {
                    val next = if (rnd.nextDouble() < 0.85 && pages(current).links.nonEmpty) {
                        rnd.shuffle(pages(current).links).head
                    } else {
                        rnd.shuffle(ids).head
                    }
                    val updatedCounts = counts.updatedWith(next) {
                        case Some(count) => Some(count + 1)
                        case None => Some(1)
                    }
                    walk(next, step + 1, updatedCounts)
                }
            }
            walk(startId, 0, Map(startId -> 1))
        }

        val totalCounts = (1 to users).foldLeft(Map.empty[String, Int]) { (visitCounts, _) =>
          // walkCounts is a master map containing the counts for how often a page was visited across all 10,000 users
          val walkCounts = randomWalk(rnd.shuffle(ids).head)
          // the second foldLeft merges the results of randomWalk into the master
          // master is visitCounts merged with walkCounts, and we repeat that merge for each walkCounts that we get
          walkCounts.foldLeft(visitCounts) { case (master, (id, count)) =>
            master.updatedWith(id) {
                case Some(c) => Some(c + count)
                case None => Some(count)
            }
          }
        }

        // rank all the pages, even if they were never visited
        val denom = users + pages.size.toDouble
        val pageRanks = pages.keys.map { id =>
          val count = totalCounts.getOrElse(id, 0)
          id -> (count / denom)
        }.toMap
        pageRanks
    }
}