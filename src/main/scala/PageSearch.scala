//import scala.collection.parallel.CollectionConverters.*
import scala.math.log

object PageSearch {
    /**
     * @param pages  a list of RankedWebPage objects to be searched
     * @param query  a list of search terms to be counted in those pages
     * @return       a list of the number of times any of the terms appeared in each page in the same order as given
     */
    def count(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        // helper method to count occurrences of a substring
        def helper(text: String, query: String): Double = {
            text.sliding(query.length).count(substring => substring == query)
        }
        for page <- pages yield {
            (for word <- query yield {
                helper(page.text, word)
            }).sum
        }
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the term-frequency of the occurrences of those terms in each page in the same order given
     */
    def tf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        pages.map { page =>
            val totalChars = page.text.length.toDouble
            if (totalChars == 0) 0.0 else query.map(term => count(List(page), List(term)).head).sum / totalChars
        }
    }

    /**
     * @param pages a list of RankedWebPage objects to be searched
     * @param query a list of search terms to be counted in those pages
     * @return      a list of the TF-IDF score for each page in the same order given
     */
    def tfidf(pages: List[RankedWebPage], query: List[String]): List[Double] = {
        val docCount = pages.length.toDouble
        val docFrequency = query.map { term =>
            pages.count(page => page.text.contains(term)).toDouble
        }
        val idf = docFrequency.map(df => if (df == 0) 0.0 else log(docCount / df))
    
        pages.map { page =>
            val tfScores = query.map(term => count(List(page), List(term)).head)
            tfScores.zip(idf).map { case (tfScore, idfScore) => tfScore * idfScore }.sum
        }
    }
}