package repository

import java.io.File

import org.tmatesoft.hg.core.{HgChangeset, HgRepoFacade, HgRepositoryLockException}

import scala.collection.JavaConversions

/**
 *
 * Created by Marcello Steiner on 27/05/15.
 */
object HgAnalyzer {

  val hgRepo = new HgRepoFacade()

  def initRepositoryAnalyzer(rootFolder: File): Unit = {
    if (!hgRepo.initFrom(rootFolder)) {
      println("Not a valid Mercurial repository")
      System.exit(1)
    }
  }

  def getRevisions: Iterable[HgChangeset] = {
    require(hgRepo.getRepository != null)
    val changeSets = hgRepo.createLogCommand().execute()
    JavaConversions.collectionAsScalaIterable(changeSets)
  }

  def checkoutRevision(revision: HgChangeset): Unit = {
    val lock = hgRepo.getRepository.getWorkingDirLock
    try {
      lock.acquire()
      hgRepo
        .createCheckoutCommand()
        .changeset(revision.getNodeid)
        .clean(true)
        .execute()
      println(s"Analyzing revision ${revision.getRevisionIndex}")
    }
    catch {
      case e: HgRepositoryLockException => e.printStackTrace()
    }
    finally lock.release()

  }


}
