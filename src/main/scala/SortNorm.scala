import scala.math

// NOTE: This object is implemented for you and does not need to be adjusted
object SearchedWebPageNormalize {
    /**
     * Transforms a list of SearchedWebPage to have new normalized values for weight and textmatch
     * @param pages a list of SearchedWebPage objects that have not been normalized
     * @return      a list of SearchedWebPage objects that have been normalized
     */
    def normalize(pages: List[SearchedWebPage]): List[SearchedWebPage] = {
        // extract lists of weights and matches from the pages
        val weights = (for page <- pages yield page.weight)
        val matches = (for page <- pages yield page.textmatch)

        // use minimum and maximum values to create a normalizing closures
        def normWeight(x: Double): Double = (x-weights.min)/(weights.max-weights.min+0.00001)
        def normMatch(x: Double): Double = (x-matches.min)/(matches.max-matches.min+0.00001)

        // use these helper functions to normalize pages
        (for (page <- pages) yield 
            new SearchedWebPage(
                page.id,
                page.name,
                page.url,
                page.text,
                page.links,
                normWeight(page.weight),
                normMatch(page.textmatch)
            )
        )
    }
}

// NOTE: This is a bogus sorting option that ignores our search weights and just uses alphabetical order on name
// This implementation exists only to give you an idea of what is required in your other objects
object NameOrdering extends Ordering[SearchedWebPage] {
    def compare(a: SearchedWebPage, b: SearchedWebPage): Int = a.name compare b.name
}

object ArithmeticOrdering extends Ordering[SearchedWebPage] {
    def compare(a: SearchedWebPage, b: SearchedWebPage): Int = {
        val a_avg = (a.textmatch + a.weight)/2
        val b_avg = (b.textmatch + b.weight)/2
        if a_avg > b_avg then 1 else if a_avg < b_avg then -1 else 0
    }
}

object GeometricOrdering extends Ordering[SearchedWebPage] {
    def compare(a: SearchedWebPage, b: SearchedWebPage): Int = {
        val a_avg = Math.sqrt(a.textmatch * a.weight)
        val b_avg = Math.sqrt(b.textmatch * b.weight)
        if a_avg > b_avg then 1 else if a_avg < b_avg then -1 else 0
    }
}

object HarmonicOrdering extends Ordering[SearchedWebPage] {
    def compare(a: SearchedWebPage, b: SearchedWebPage): Int = {
        val a_avg = (2 * a.textmatch * a.weight)/(a.textmatch + a.weight)
        val b_avg = (2 * b.textmatch * b.weight)/(b.textmatch + b.weight)
        if a_avg > b_avg then 1 else if a_avg < b_avg then -1 else 0
    }
}
