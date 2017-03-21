package evmap
import net.ruippeixotog.scalascraper.browser.JsoupBrowser
import net.ruippeixotog.scalascraper.dsl.DSL._
import net.ruippeixotog.scalascraper.dsl.DSL.Extract._
import net.ruippeixotog.scalascraper.dsl.DSL.Parse._
import net.ruippeixotog.scalascraper.model._
import scala.collection.mutable._
import java.util.concurrent.LinkedBlockingQueue


object Hello extends App {
  var handledlinks = Set[String]()
  val queue = new LinkedBlockingQueue[String]()
  val docs = new LinkedBlockingQueue[Document]()
  var consumerStart = false
  val baseUrl = "http://divideby.ru/"
  val lockq = new AnyRef
  val lockd = new AnyRef

  object Producer extends Thread {
    override def run() = {
      val t1 = thread(prodWork)
      t1.join
    }

    def prodWork = {
      while (true) {
        if (!queue.isEmpty) {
          val link = queue.take()
          docs.put(download(link))
          handledlinks += link
          println(s"docs: ${docs.size}")
          println(s"links: ${handledlinks.size}")
          println(s"queue: ${queue.size}")
        }
      }
    }

  }


  object Consumer extends Thread {
    override def run() {
      val t1 = thread(consWork)
      val t2 = thread(consWork)
      val t3 = thread(consWork)
      t1.join
      t2.join
      t3.join
    }

    def consWork = {
      while(true) {
        if(!docs.isEmpty) {
          val doc = docs.take()
          extractLinks(doc)
        }
      }
    }
  }

  def thread(body: =>Unit): Thread = {
    val t = new Thread {
      override def run() = body
    }
    t.start()
    t
  }


  def start = {
    queue.put(baseUrl)
    Producer.start
    Consumer.start
  }

  def download(url: String): Document = {
    //для прокси
    //ProxyUtils.setProxy("http://proxy.isu.ru", 3128)
    val browser = JsoupBrowser()
    val doc = browser.get(url)
    //println(doc)
    doc
  }

  def extractLinks(doc: Document) = {
    val items: List[Element] = doc >> elementList("a")
    for (item <- items) {
      if (item.hasAttr("href")) {
        val link = item.attr("abs:href")
        if (link.contains(baseUrl) && !handledlinks.contains(link))
          queue.put(link)
      }
    }
  }

  start

}

