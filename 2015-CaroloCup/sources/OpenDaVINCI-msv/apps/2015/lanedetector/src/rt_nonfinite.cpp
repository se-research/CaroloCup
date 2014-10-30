/*
 * rt_nonfinite.c
 *
 * Code generation for function 'nnRoadAngleCalc'
 *
 * C source code generated on: Tue Jan 28 02:26:12 2014
 *
 */

/*
 * Abstract:
 *      MATLAB for code generation function to initialize non-finites,
 *      (Inf, NaN and -Inf).
 */
#include "rt_nonfinite.h"
#include "rtGetNaN.h"
#include "rtGetInf.h"
#include "math.h"
#include "cmath"

real_T rtInf;
real_T rtMinusInf;
real_T rtNaN;
real32_T rtInfF;
real32_T rtMinusInfF;
real32_T rtNaNF;

/* Function: rt_InitInfAndNaN ==================================================
 * Abstract:
 * Initialize the rtInf, rtMinusInf, and rtNaN needed by the
 * generated code. NaN is initialized as non-signaling. Assumes IEEE.
 */
void rt_InitInfAndNaN(size_t realSize)
{
  (void) (realSize);
  rtNaN = rtGetNaN();
  rtNaNF = rtGetNaNF();
  rtInf = rtGetInf();
  rtInfF = rtGetInfF();
  rtMinusInf = rtGetMinusInf();
  rtMinusInfF = rtGetMinusInfF();
}

/* Function: rtIsInf ==================================================
 * Abstract:
 * Test if value is infinite
 */
boolean_T rtIsInf(real_T value)
{ // Use error margin to avoid "float-equal" warnings
	if ( fabs(value - rtInf) < 0.00001  || fabs(value - rtMinusInf) < 0.00001 )
		return 1U;
	else
		return 0U;
  //return ((value==rtInf || value==rtMinusInf) ? 1U : 0U);
}

/* Function: rtIsInfF =================================================
 * Abstract:
 * Test if single-precision value is infinite
 */
boolean_T rtIsInfF(real32_T value)
{
	if ( fabs(value - rtInfF) < 0.00001  || fabs(value - rtMinusInfF) < 0.00001 )
			return 1U;
		else
			return 0U;
  //return(((value)==rtInfF || (value)==rtMinusInfF) ? 1U : 0U);
}

/* Function: rtIsNaN ==================================================
 * Abstract:
 * Test if value is not a number
 */
boolean_T rtIsNaN(real_T value)
{
#if defined(_MSC_VER) && (_MSC_VER <= 1200)
  return _isnan(value)? TRUE:FALSE;
#else
  return std::isnan(value) ? 1U:0U;
  //return (value!=value)? 1U:0U;
#endif
}

/* Function: rtIsNaNF =================================================
 * Abstract:
 * Test if single-precision value is not a number
 */
boolean_T rtIsNaNF(real32_T value)
{
#if defined(_MSC_VER) && (_MSC_VER <= 1200)
  return _isnan((real_T)value)? true:false;
#else
  // Check if "value" is indeterminate or infinite
  return (std::isnan(value) || std::isinf(value)) ?  1U:0U;
  //return (value!=value)? 1U:0U;
#endif
}


/* End of code generation (rt_nonfinite.c) */
