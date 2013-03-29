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

import sessl._
import sessl.AbstractExperiment
import sessl.ExperimentConfiguration
import sessl.util.ScalaToJava._
import umontreal.iro.lecuyer.functionfit.LeastSquares
import umontreal.iro.lecuyer.functionfit.BSpline
import umontreal.iro.lecuyer.functions.MathFunction
import umontreal.iro.lecuyer.functions.Polynomial
import umontreal.iro.lecuyer.functionfit.SmoothingCubicSpline

/**
 * Provides support for output analysis with SSJ.
 *
 * See SSJ manual for documentation: http://www.iro.umontreal.ca/~simardr/ssj/doc/pdf/guidefunctionfit.pdf
 *
 * @author Roland Ewald
 */
trait SSJOutputAnalysis extends ExperimentConfiguration {
  this: AbstractExperiment =>

  def fitPolynom(data: Trajectory, degree: Int): Polynomial = {
    checkTrajectoryValidity(data)
    val converted = convertValues(data)
    new LeastSquares(converted._1, converted._2, degree)
  }

  def fitBSpline(data: Trajectory, degree: Int): BSpline = {
    checkTrajectoryValidity(data)
    val converted = convertValues(data)
    BSpline.createInterpBSpline(converted._1, converted._2, degree)
  }

  def fitApproxBSpline(data: Trajectory, degree: Int, h: Int): BSpline = {
    checkTrajectoryValidity(data)
    val converted = convertValues(data)
    BSpline.createApproxBSpline(converted._1, converted._2, degree, h)
  }

  def fitCubicSpline(data: Trajectory, rho: Double): SmoothingCubicSpline = {
    require(rho >= 0 && rho <= 1, "Parameter rho needs to be in [0,1].")
    checkTrajectoryValidity(data)
    val converted = convertValues(data)
    new SmoothingCubicSpline(converted._1, converted._2, rho)
  }

  private[this] def convertValues(t: Trajectory): (Array[Double], Array[Double]) =
    (t.map(_._1).toArray, t.map(_._2.asInstanceOf[Number].doubleValue).toArray)

  private[this] def checkTrajectoryValidity(t: Trajectory) = require(t.head._2.isInstanceOf[Number], "Trajectory values need to be real-valued.")

}