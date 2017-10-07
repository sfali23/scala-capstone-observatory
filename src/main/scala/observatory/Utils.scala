package observatory

/**
  * @author sali
  */
object Utils {

  def convertTime(nanoSeconds: Long): String = {
    val ms = nanoSeconds / 1000000
    val milliSeconds = ms % 1000

    val sec = ms / 1000
    val seconds = sec % 60

    val min = sec / 60
    val minutes = min % 60

    val hours = min / 60
    val builder = new StringBuilder
    if (hours > 0L) builder.append(hours).append(" hours ")
    if (minutes > 0L) builder.append(minutes).append(" minutes ")
    builder.append(seconds).append(" seconds ").append(milliSeconds).append(" ms ")
    builder.toString()
  }
}
