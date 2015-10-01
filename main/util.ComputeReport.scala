package util

trait ComputeReport[+Report] {
  def computeReport(): Report
}
