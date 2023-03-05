package zio.sql.utils

import scala.util.matching.Regex

object Pluralize {

  def pluralize(word0: String): String = {
    val word = word0.toLowerCase
    if (irregularSingles.contains(word.toLowerCase())) {
      irregularSingles(word.toLowerCase())
    } else if (noChange(word)) {
      word0
    } else {
      findLast(pluralRules) { case (regex, _) =>
        regex.findFirstMatchIn(word).fold(false)(_ => true)
      }.flatMap { case (regex, s) =>
        regex.findFirstMatchIn(word).map(mtch => (regex, s, mtch))
      }.map { case (regex, s, mtch) =>
        val group = if (mtch.start > 0 && mtch.start < word.length - 1) {
          scala.util.Try(mtch.group(1)).fold(_ => None, g => Option(g))
        } else {
          None
        }

        val result = group match {
          case Some(group) =>
            "\\$(\\d{1,2})".r.replaceFirstIn(s, group)
          case None        =>
            s
        }

        regex.replaceFirstIn(word, result)
      } match {
        case Some(value) => value
        case None        => word0
      }
    }
  }

  private def findLast[A](la: List[A])(f: A => Boolean): Option[A] =
    la.foldLeft(Option.empty[A]) { (acc, cur) =>
      if (f(cur)) Some(cur)
      else acc
    }

  def isSingular(word: String): Boolean =
    !irregularPlurals.contains(word.toLowerCase) ||
      (singularRules
        .map(_._1)
        .flatMap(_.findFirstMatchIn(word.toLowerCase()).fold(List.empty[Regex.Match])(_ :: Nil)) match {
        case _ :: _ => true
        case Nil    => false
      })

  private def noChange(word: String): Boolean =
    noPlural.contains(word) || noPluralRegex
      .find(_.findFirstMatchIn(word).fold(false)(_ => true))
      .fold(false)(_ => true)

  private val pluralRules: List[(Regex, String)] = List(
    "s?$".r                                                                                                                  -> "s",
    "[^\u0000-\u007F]$".r.unanchored                                                                                         -> "$0",
    "([^aeiou]ese)$".r                                                                                                       -> "$1",
    "(ax|test)is$".r                                                                                                         -> "$1es",
    "(alias|[^aou]us|t[lm]as|gas|ris)$".r                                                                                    -> "$1es",
    "(e[mn]u)s?$".r                                                                                                          -> "$1s",
    "([^l]ias|[aeiou]las|[ejzr]as|[iu]am)$".r                                                                                -> "$1",
    "(alumn|syllab|vir|radi|nucle|fung|cact|stimul|termin|bacill|foc|uter|loc|strat)(?:us|i)$".r                             -> "$1i",
    "(alumn|alg|vertebr)(?:a|ae)$".r                                                                                         -> "$1ae",
    "(seraph|cherub)(?:im)?$".r                                                                                              -> "$1im",
    "(her|at|gr)o$".r                                                                                                        -> "$1oes",
    "(agend|addend|millenni|dat|extrem|bacteri|desiderat|strat|candelabr|errat|ov|symposi|curricul|automat|quor)(?:a|um)$".r -> "$1a",
    "(apheli|hyperbat|periheli|asyndet|noumen|phenomen|criteri|organ|prolegomen|hedr|automat)(?:a|on)$".r                    -> "$1a",
    "sis$".r                                                                                                                 -> "ses",
    "(?:(kni|wi|li)fe|(ar|l|ea|eo|oa|hoo)f)$".r                                                                              -> "$1$2ves",
    "([^aeiouy]|qu)y$".r                                                                                                     -> "$1ies",
    "([^ch][ieo][ln])ey$".r                                                                                                  -> "$1ies",
    "(x|ch|ss|sh|zz)$".r                                                                                                     -> "$1es",
    "(matr|cod|mur|sil|vert|ind|append)(?:ix|ex)$".r                                                                         -> "$1ices",
    "^((?:tit)?m|l)(?:ice|ouse)$".r                                                                                          -> "$1ice",
    "(pe)(?:rson|ople)$".r                                                                                                   -> "$1ople",
    "(child)(?:ren)?$".r                                                                                                     -> "$1ren",
    "eaux$".r                                                                                                                -> "$0",
    "m[ae]n$".r                                                                                                              -> "men"
  )

  private val irregularPlurals = Map(
    "we"         -> "i",
    "us"         -> "me",
    "they"       -> "she",
    "them"       -> "them",
    "ourselves"  -> "myself",
    "yourselves" -> "yourself",
    "themselves" -> "themself",
    "are"        -> "is",
    "were"       -> "was",
    "have"       -> "has",
    "these"      -> "this",
    "those"      -> "that",
    "echoes"     -> "echo",
    "dingoes"    -> "dingo",
    "volcanoes"  -> "volcano",
    "tornadoes"  -> "tornado",
    "torpedoes"  -> "torpedo",
    "genera"     -> "genus",
    "viscera"    -> "viscus",
    "stigmata"   -> "stigma",
    "stomata"    -> "stoma",
    "dogmata"    -> "dogma",
    "lemmata"    -> "lemma",
    "schemata"   -> "schema",
    "anathemata" -> "anathema",
    "oxen"       -> "ox",
    "axes"       -> "axe",
    "dice"       -> "die",
    "yeses"      -> "yes",
    "feet"       -> "foot",
    "eaves"      -> "eave",
    "geese"      -> "goose",
    "teeth"      -> "tooth",
    "quizzes"    -> "quiz",
    "humans"     -> "human",
    "proofs"     -> "proof",
    "carves"     -> "carve",
    "valves"     -> "valve",
    "looies"     -> "looey",
    "thieves"    -> "thief",
    "grooves"    -> "groove",
    "pickaxes"   -> "pickaxe",
    "passersby"  -> "passerby"
  )

  private val irregularSingles = Map(
    "i"        -> "we",
    "me"       -> "us",
    "he"       -> "they",
    "she"      -> "they",
    "them"     -> "them",
    "myself"   -> "ourselves",
    "yourself" -> "yourselves",
    "itself"   -> "themselves",
    "herself"  -> "themselves",
    "himself"  -> "themselves",
    "themself" -> "themselves",
    "is"       -> "are",
    "was"      -> "were",
    "has"      -> "have",
    "this"     -> "these",
    "that"     -> "those",
    "echo"     -> "echoes",
    "dingo"    -> "dingoes",
    "volcano"  -> "volcanoes",
    "tornado"  -> "tornadoes",
    "torpedo"  -> "torpedoes",
    "genus"    -> "genera",
    "viscus"   -> "viscera",
    "stigma"   -> "stigmata",
    "stoma"    -> "stomata",
    "dogma"    -> "dogmata",
    "lemma"    -> "lemmata",
    "schema"   -> "schemata",
    "anathema" -> "anathemata",
    "ox"       -> "oxen",
    "axe"      -> "axes",
    "die"      -> "dice",
    "yes"      -> "yeses",
    "foot"     -> "feet",
    "eave"     -> "eaves",
    "goose"    -> "geese",
    "tooth"    -> "teeth",
    "quiz"     -> "quizzes",
    "human"    -> "humans",
    "proof"    -> "proofs",
    "carve"    -> "carves",
    "valve"    -> "valves",
    "looey"    -> "looies",
    "thief"    -> "thieves",
    "groove"   -> "grooves",
    "pickaxe"  -> "pickaxes",
    "passerby" -> "passersby"
  )

  private val singularRules = List(
    "s$".r                                                                                                         -> "",
    "(ss)$".r                                                                                                      -> "$1",
    "(wi|kni|(?:after|half|high|low|mid|non|night|[^\\w]|^)li)ves$".r                                              -> "$1fe",
    "(ar|(?:wo|[ae])l|[eo][ao])ves$".r                                                                             -> "$1f",
    "ies$".r                                                                                                       -> "y",
    "(dg|ss|ois|lk|ok|wn|mb|th|ch|ec|oal|is|ck|ix|sser|ts|wb)ies$".r                                               -> "$1ie",
    "\b(l|(?:neck|cross|hog|aun)?t|coll|faer|food|gen|goon|group|hipp|junk|vegg|(?:pork)?p|charl|calor|cut)ies$".r -> "$1ie",
    "\b(mon|smil)ies$".r                                                                                           -> "$1ey",
    "\b((?:tit)?m|l)ice$".r                                                                                        -> "$1ouse",
    "(seraph|cherub)im$".r                                                                                         -> "$1",
    "(x|ch|ss|sh|zz|tto|go|cho|alias|[^aou]us|t[lm]as|gas|(?:her|at|gr)o|[aeiou]ris)(?:es)?$".r                    -> "$1",
    "(analy|diagno|parenthe|progno|synop|the|empha|cri|ne)(?:sis|ses)$".r                                          -> "$1sis",
    "(movie|twelve|abuse|e[mn]u)s$".r                                                                              -> "$1",
    "(test)(?:is|es)$".r                                                                                           -> "$1is",
    "(alumn|syllab|vir|radi|nucle|fung|cact|stimul|termin|bacill|foc|uter|loc|strat)(?:us|i)$".r                   -> "$1us",
    "(agend|addend|millenni|dat|extrem|bacteri|desiderat|strat|candelabr|errat|ov|symposi|curricul|quor)a$".r      -> "$1um",
    "(apheli|hyperbat|periheli|asyndet|noumen|phenomen|criteri|organ|prolegomen|hedr|automat)a$".r                 -> "$1on",
    "(alumn|alg|vertebr)ae$".r                                                                                     -> "$1a",
    "(cod|mur|sil|vert|ind)ices$".r                                                                                -> "$1ex",
    "(matr|append)ices$".r                                                                                         -> "$1ix",
    "(pe)(rson|ople)$".r                                                                                           -> "$1rson",
    "(child)ren$".r                                                                                                -> "$1",
    "(eau)x?$".r                                                                                                   -> "$1",
    "men$".r                                                                                                       -> "man"
  )

  private val noPluralRegex = List(
    "[^aeiou]ese$".r, // "chinese", "japanese"
    "deer$".r,        // "deer", "reindeer"
    "fish$".r,        // "fish", "blowfish", "angelfish"
    "measles$".r,     // "carnivorous"
    "o[iu]s$".r,      // "chickpox", "smallpox"
    "pox$".r,
    "sheep$".r
  )

  private val noPlural = List(
    "adulthood",
    "advice",
    "agenda",
    "aid",
    "aircraft",
    "alcohol",
    "ammo",
    "analytics",
    "anime",
    "athletics",
    "audio",
    "bison",
    "blood",
    "bream",
    "buffalo",
    "butter",
    "carp",
    "cash",
    "chassis",
    "chess",
    "clothing",
    "cod",
    "commerce",
    "cooperation",
    "corps",
    "debris",
    "diabetes",
    "digestion",
    "elk",
    "energy",
    "equipment",
    "excretion",
    "expertise",
    "firmware",
    "flounder",
    "fun",
    "gallows",
    "garbage",
    "graffiti",
    "hardware",
    "headquarters",
    "health",
    "herpes",
    "highjinks",
    "homework",
    "housework",
    "information",
    "jeans",
    "justice",
    "kudos",
    "labour",
    "literature",
    "machinery",
    "mackerel",
    "mail",
    "media",
    "mews",
    "moose",
    "music",
    "mud",
    "manga",
    "news",
    "only",
    "personnel",
    "pike",
    "plankton",
    "pliers",
    "police",
    "pollution",
    "premises",
    "rain",
    "research",
    "rice",
    "salmon",
    "scissors",
    "series",
    "sewage",
    "shambles",
    "shrimp",
    "software",
    "species",
    "staff",
    "swine",
    "tennis",
    "traffic",
    "transportation",
    "trout",
    "tuna",
    "wealth",
    "welfare",
    "whiting",
    "wildebeest",
    "wildlife",
    "you",
    "pokemon",
    "pokémon",
    "deer",
    "reindeer",
    "fish",
    "fish",
    "blowfish",
    "angelfish",
    "measles",
    "carnivorous",
    "pox",
    "chickpox",
    "smallpox",
    "sheep"
  )
}
