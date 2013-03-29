/**
 * Copyright (C) 2013 Roland Ewald
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package sessl.ssj

import com.weiglewilczek.slf4s.Logger

import sessl.AbstractExperiment
import sessl.AbstractReport
import sessl.BoxPlotView
import sessl.DataView
import sessl.ExperimentResults
import sessl.HistogramView
import sessl.LinePlotView
import sessl.ReportNode
import sessl.ReportSectionNode
import sessl.ScatterPlotView
import umontreal.iro.lecuyer.charts.ScatterChart

/**
 * @author Roland Ewald
 */
trait Report extends AbstractReport {
  this: AbstractExperiment =>

  def generateReport(results: ExperimentResults): Unit = {
    topmostElements.foreach(e => createView(reportName, e))
  }

  /** Creates top-most elements of report (adds dummy sections to data views on top-most level). */
  def createView(name: String, node: ReportNode): Unit = node match {
    case section: ReportSectionNode => {
      section.children.foreach(e => createView(name + "-" + section.name, e))
      if (!section.description.isEmpty)
        logger.warn("Can't display sections, omitting description '" + section.description + "'")
    }
    case view: DataView => createChart(name, view)
    case _ => throw new IllegalArgumentException("Element " + node + " not supported.")
  }

  private[this] def createChart(name: String, view: DataView): Unit = {
    import sessl.util.ScalaToJava._
    view match {
      case v: ScatterPlotView => {
        val twoDArray: Array[Array[Double]] = (v.xData zip v.yData).toArray.map(x => Array(x._1, x._2))
        new ScatterChart(v.title, v.xLabel, v.yLabel, twoDArray, 1, 1)
      }
      case v: HistogramView => null
      case v: BoxPlotView => null
      case v: LinePlotView => null
      case _ => throw new IllegalArgumentException("Data view " + view + " not yet supported.")
    }
  }

}