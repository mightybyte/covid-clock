{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module SEIR where

import GHC.Generics

data Params = Params
  { _params_ttd :: Double -- time to death
  , _params_i0 :: Double
  , _params_r0 :: Double
  , _params_dIncubation :: Double
  , _params_dInfectious :: Double
  , _params_dRecoveryMild :: Double
  , _params_dRecoverySevere :: Double
  , _params_dHospitalLag :: Double
  , _params_dDeath :: Double
  , _params_cfr :: Double -- fatality rate
  , _params_interventionTime :: Double
  , _params_interventionAmount :: Double
  , _params_pSevere :: Double
  , _params_duration :: Double
  } deriving (Eq,Ord,Show,Read,Generic)

defParams = Params
    { _params_ttd = ttd
    , _params_i0 = 1
    , _params_r0 = 2.2
    , _params_dIncubation = 5.2
    , _params_dInfectious = infectious
    , _params_dRecoveryMild = 14 - 2.9
    , _params_dRecoverySevere = 31.5 - 2.9
    , _params_dHospitalLag = 5
    , _params_dDeath = ttd - infectious
    , _params_cfr = 0.02
    , _params_interventionTime = 100
    , _params_interventionAmount = 1.0 / 3.0
    , _params_pSevere = 0.2
    , _params_duration = 7 * 12 * 1e10
    }
  where
    ttd = 32
    infectious = 2.9

data DiseaseModel = DiseaseModel
  { _dm_s :: Double
  , _dm_e :: Double
  , _dm_i :: Double
  , _dm_mild :: Double
  , _dm_severe :: Double
  , _dm_severeH :: Double
  , _dm_fatal :: Double
  , _dm_rMild :: Double
  , _dm_rSevere :: Double
  , _dm_rFatal :: Double
  } deriving (Eq,Ord,Show,Read,Generic)

type Time = Double

calcDerivative :: Params -> DiseaseModel -> Time -> DiseaseModel
calcDerivative Params{..} DiseaseModel{..} t = DiseaseModel
    { _dm_s = negate bis
    , _dm_e = bis - a * _dm_e
    , _dm_i = a * _dm_e - gamma * _dm_i
    , _dm_mild = pMild * gamma * _dm_i - _dm_mild / _params_dRecoveryMild
    , _dm_severe = _params_pSevere * gamma * _dm_i - (_dm_severe / _params_dHospitalLag)
    , _dm_severeH = _dm_severe / _params_dHospitalLag - _dm_severeH / _params_dRecoverySevere
    , _dm_fatal = _params_cfr * gamma * _dm_i - _dm_fatal / _params_dDeath
    , _dm_rMild = _dm_mild / _params_dRecoveryMild
    , _dm_rSevere = _dm_severeH / _params_dRecoverySevere
    , _dm_rFatal = _dm_fatal / _params_dDeath
    }
  where
    beta = if t > _params_interventionTime && t < _params_interventionTime + _params_duration
             then _params_interventionAmount * _params_r0 / _params_dInfectious
             else if t > _params_interventionTime + _params_duration
               then 0.5 * _params_r0 / _params_dInfectious
               else _params_r0 / _params_dInfectious
    a = 1.0 / _params_dIncubation
    gamma = 1.0 / _params_dInfectious
    pMild = 1 - _params_pSevere - _params_cfr
    bis = beta * _dm_i * _dm_s

data Solution = Solution
  { _sol_p :: [Double]
  , _sol_deaths :: Double
  , _sol_total :: Double
  , _sol_totalInfected :: Double
  , _sol_iters :: Int
  }

--getSolution st

{-
  var sum = function(arr, bools){
    var x = 0
    for (var i = 0; i < arr.length; i++) {
      x = x + arr[i]*(bools[i] ? 1 : 0)
    }
    return x
  }
  var Integrators = {
    Euler    : [[1]],
    Midpoint : [[.5,.5],[0, 1]],
    Heun     : [[1, 1],[.5,.5]],
    Ralston  : [[2/3,2/3],[.25,.75]],
    K3       : [[.5,.5],[1,-1,2],[1/6,2/3,1/6]],
    SSP33    : [[1,1],[.5,.25,.25],[1/6,1/6,2/3]],
    SSP43    : [[.5,.5],[1,.5,.5],[.5,1/6,1/6,1/6],[1/6,1/6,1/6,1/2]],
    RK4      : [[.5,.5],[.5,0,.5],[1,0,0,1],[1/6,1/3,1/3,1/6]],
    RK38     : [[1/3,1/3],[2/3,-1/3,1],[1,1,-1,1],[1/8,3/8,3/8,1/8]]
  };
  // f is a func of time t and state y
  // y is the initial state, t is the time, h is the timestep
  // updated y is returned.
  var integrate=(m,f,y,t,h)=>{
    for (var k=[],ki=0; ki<m.length; ki++) {
      var _y=y.slice(), dt=ki?((m[ki-1][0])*h):0;
      for (var l=0; l<_y.length; l++) for (var j=1; j<=ki; j++) _y[l]=_y[l]+h*(m[ki-1][j])*(k[ki-1][l]);
      k[ki]=f(t+dt,_y,dt);
    }
    for (var r=y.slice(),l=0; l<_y.length; l++) for (var j=0; j<k.length; j++) r[l]=r[l]+h*(k[j][l])*(m[ki-1][j]);
    return r;
  }
  $: Time_to_death     = 32
  $: logN              = Math.log(7e6)
  $: N                 = Math.exp(logN)
  $: I0                = 1
  $: R0                = 2.2
  $: D_incbation       = 5.2
  $: D_infectious      = 2.9
  $: D_recovery_mild   = (14 - 2.9)
  $: D_recovery_severe = (31.5 - 2.9)
  $: D_hospital_lag    = 5
  $: D_death           = Time_to_death - D_infectious
  $: CFR               = 0.02
  $: InterventionTime  = 100
  $: InterventionAmt   = 1/3
  $: Time              = 220
  $: Xmax              = 110000
  $: dt                = 2
  $: P_SEVERE          = 0.2
  $: duration          = 7*12*1e10
  $: state = location.protocol + '//' + location.host + location.pathname + "?" + queryString.stringify({"Time_to_death":Time_to_death,
               "logN":logN,
               "I0":I0,
               "R0":R0,
               "D_incbation":D_incbation,
               "D_infectious":D_infectious,
               "D_recovery_mild":D_recovery_mild,
               "D_recovery_severe":D_recovery_severe,
               "CFR":CFR,
               "InterventionTime":InterventionTime,
               "InterventionAmt":InterventionAmt,
               "D_hospital_lag":D_hospital_lag,
               "P_SEVERE": P_SEVERE})
  function get_solution(dt, N, I0, R0, D_incbation, D_infectious, D_recovery_mild, D_hospital_lag, D_recovery_severe, D_death, P_SEVERE, CFR, InterventionTime, InterventionAmt, duration) {
    var interpolation_steps = 40
    var steps = 110*interpolation_steps
    var dt = dt/interpolation_steps
    var sample_step = interpolation_steps
    var method = Integrators["RK4"]
    function f(t, x){
      // SEIR ODE
      if (t > InterventionTime && t < InterventionTime + duration){
        var beta = (InterventionAmt)*R0/(D_infectious)
      } else if (t > InterventionTime + duration) {
        var beta = 0.5*R0/(D_infectious)
      } else {
        var beta = R0/(D_infectious)
      }
      var a     = 1/D_incbation
      var gamma = 1/D_infectious

      var S        = x[0] // Susectable
      var E        = x[1] // Exposed
      var I        = x[2] // Infectious
      var Mild     = x[3] // Recovering (Mild)
      var Severe   = x[4] // Recovering (Severe at home)
      var Severe_H = x[5] // Recovering (Severe in hospital)
      var Fatal    = x[6] // Recovering (Fatal)
      var R_Mild   = x[7] // Recovered
      var R_Severe = x[8] // Recovered
      var R_Fatal  = x[9] // Dead
      var p_severe = P_SEVERE
      var p_fatal  = CFR
      var p_mild   = 1 - P_SEVERE - CFR
      var dS        = -beta*I*S
      var dE        =  beta*I*S - a*E
      var dI        =  a*E - gamma*I
      var dMild     =  p_mild*gamma*I   - (1/D_recovery_mild)*Mild
      var dSevere   =  p_severe*gamma*I - (1/D_hospital_lag)*Severe
      var dSevere_H =  (1/D_hospital_lag)*Severe - (1/D_recovery_severe)*Severe_H
      var dFatal    =  p_fatal*gamma*I  - (1/D_death)*Fatal
      var dR_Mild   =  (1/D_recovery_mild)*Mild
      var dR_Severe =  (1/D_recovery_severe)*Severe_H
      var dR_Fatal  =  (1/D_death)*Fatal
      //      0   1   2   3      4        5          6       7        8          9
      return [dS, dE, dI, dMild, dSevere, dSevere_H, dFatal, dR_Mild, dR_Severe, dR_Fatal]
    }
    var v = [1, 0, I0/(N-I0), 0, 0, 0, 0, 0, 0, 0]
    var t = 0
    var P  = []
    var TI = []
    var Iters = []
    while (steps--) {
      if ((steps+1) % (sample_step) == 0) {
            //    Dead   Hospital          Recovered        Infected   Exposed
        P.push([ N*v[9], N*(v[5]+v[6]),  N*(v[7] + v[8]), N*v[2],    N*v[1] ])
        Iters.push(v)
        TI.push(N*(1-v[0]))
        // console.log((v[0] + v[1] + v[2] + v[3] + v[4] + v[5] + v[6] + v[7] + v[8] + v[9]))
        // console.log(v[0] , v[1] , v[2] , v[3] , v[4] , v[5] , v[6] , v[7] , v[8] , v[9])
      }
      v =integrate(method,f,v,t,dt);
      t+=dt
    }
    return {"P": P,
            "deaths": N*v[6],
            "total": 1-v[0],
            "total_infected": TI,
            "Iters":Iters,
            "dIters": f}
  }
-}
