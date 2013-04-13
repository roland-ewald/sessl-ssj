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

import sessl.Trajectory
import umontreal.iro.lecuyer.functionfit.LeastSquares
import umontreal.iro.lecuyer.functions.MathFunction
import sessl.Misc

/**
 * Constructs to represent approximation forms.
 * @author Roland Ewald
 */
trait ApproximationForm[+M <: MathFunction] {

  /** Needs to be implemented by the approximation form. */
  def approximate(times: Array[Double], values: Array[Double]): M

  /** Converts and checks SESSL data for usage, the fits approximation form. */
  def fitToData(data: Trajectory): M = {
    Misc.requireNumericTrajectories(data)
    val converted = convertValues(data)
    approximate(converted._1, converted._2)
  }

  /** Convert trajectory to tuple of double[] (times + values).*/
  private[this] def convertValues(t: Trajectory): (Array[Double], Array[Double]) =
    (t.map(_._1).toArray, t.map(_._2.asInstanceOf[Number].doubleValue).toArray)
}

case class Polynomial(degree: Int) extends ApproximationForm[umontreal.iro.lecuyer.functions.Polynomial] {
  override def approximate(times: Array[Double], values: Array[Double]) = new LeastSquares(times, values, degree)
}

case class BSpline(degree: Int) extends ApproximationForm[umontreal.iro.lecuyer.functionfit.BSpline] {
  override def approximate(times: Array[Double], values: Array[Double]) = umontreal.iro.lecuyer.functionfit.BSpline.createInterpBSpline(times, values, degree)
}

case class ApproximatedBSpline(degree: Int, h: Int) extends ApproximationForm[umontreal.iro.lecuyer.functionfit.BSpline] {
  override def approximate(times: Array[Double], values: Array[Double]) = umontreal.iro.lecuyer.functionfit.BSpline.createApproxBSpline(times, values, degree, h)
}

case class SmoothingCubicSpline(rho: Double) extends ApproximationForm[umontreal.iro.lecuyer.functionfit.SmoothingCubicSpline] {
  require(rho >= 0 && rho <= 1, "Parameter rho needs to be in [0,1].")
  override def approximate(times: Array[Double], values: Array[Double]) = new umontreal.iro.lecuyer.functionfit.SmoothingCubicSpline(times, values, rho)
}

