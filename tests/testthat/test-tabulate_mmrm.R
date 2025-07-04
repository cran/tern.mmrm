library(dplyr)

get_anl <- function() {
  read.table(
    header = TRUE,
    sep = ";",
    stringsAsFactors = TRUE,
    text = '
      "USUBJID";"ARM";"STRATA1";"BMRKR2";"AVISIT";"BASE";"AVAL"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"67.88985864";"50.43875924"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"67.88985864";"61.50291623"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"67.88985864";"50.68271806"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"67.88985864";"43.149968"
      "AB12345-BRA-1-id-105";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"67.88985864";"55.5516058"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 1 DAY 8";"60.02499749";"55.08326531"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 2 DAY 15";"60.02499749";"43.87093725"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 3 DAY 22";"60.02499749";"61.68408546"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 4 DAY 29";"60.02499749";"51.24487629"
      "AB12345-BRA-1-id-134";"A: Drug X";"B";"LOW";"WEEK 5 DAY 36";"60.02499749";"47.51623139"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"43.29633267";"42.46491872"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"43.29633267";"50.88502511"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"43.29633267";"51.65439907"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"43.29633267";"63.62737932"
      "AB12345-BRA-1-id-141";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"43.29633267";"68.82593785"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 1 DAY 8";"56.39948748";"58.12445932"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 2 DAY 15";"56.39948748";"45.74268906"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 3 DAY 22";"56.39948748";"62.35864011"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 4 DAY 29";"56.39948748";"54.26595123"
      "AB12345-BRA-1-id-236";"B: Placebo";"A";"HIGH";"WEEK 5 DAY 36";"56.39948748";"49.47182985"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 1 DAY 8";"62.81857503";"42.59818754"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 2 DAY 15";"62.81857503";"53.01259956"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 3 DAY 22";"62.81857503";"59.02291642"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 4 DAY 29";"62.81857503";"45.08336743"
      "AB12345-BRA-1-id-265";"C: Combination";"A";"MEDIUM";"WEEK 5 DAY 36";"62.81857503";"59.64104106"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"48.2728554";"52.92784753"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"48.2728554";"49.84078804"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"48.2728554";"49.07336306"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"48.2728554";"70.31214801"
      "AB12345-BRA-1-id-42";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"48.2728554";"41.85491921"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 1 DAY 8";"53.37767135";"55.48675537"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 2 DAY 15";"53.37767135";"58.33272863"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 3 DAY 22";"53.37767135";"35.04338482"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 4 DAY 29";"53.37767135";"48.20473285"
      "AB12345-BRA-1-id-65";"B: Placebo";"A";"MEDIUM";"WEEK 5 DAY 36";"53.37767135";"48.32984847"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 1 DAY 8";"71.71464916";"47.70996987"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 2 DAY 15";"71.71464916";"53.07874642"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 3 DAY 22";"71.71464916";"51.2307956"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 4 DAY 29";"71.71464916";"51.83555366"
      "AB12345-BRA-1-id-93";"A: Drug X";"A";"LOW";"WEEK 5 DAY 36";"71.71464916";"60.62970709"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"65.14085313";"46.73739036"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"65.14085313";"44.68338469"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"65.14085313";"45.85457862"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"65.14085313";"53.50815722"
      "AB12345-BRA-11-id-171";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"65.14085313";"49.59114575"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 1 DAY 8";"61.41400831";"68.98362472"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 2 DAY 15";"61.41400831";"49.84940525"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 3 DAY 22";"61.41400831";"51.08326489"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 4 DAY 29";"61.41400831";"50.15482906"
      "AB12345-BRA-11-id-217";"A: Drug X";"A";"MEDIUM";"WEEK 5 DAY 36";"61.41400831";"52.38921307"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"44.12324939";"43.52861587"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"44.12324939";"49.55231325"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"44.12324939";"68.20788921"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"44.12324939";"56.776013"
      "AB12345-BRA-11-id-237";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"44.12324939";"59.16521401"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 1 DAY 8";"59.58228896";"46.23169784"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 2 DAY 15";"59.58228896";"64.57419992"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 3 DAY 22";"59.58228896";"52.54657508"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 4 DAY 29";"59.58228896";"45.02543605"
      "AB12345-BRA-11-id-321";"C: Combination";"C";"HIGH";"WEEK 5 DAY 36";"59.58228896";"71.87423258"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 1 DAY 8";"52.52563791";"57.06466667"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 2 DAY 15";"52.52563791";"48.3326902"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 3 DAY 22";"52.52563791";"59.23155396"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 4 DAY 29";"52.52563791";"44.54233766"
      "AB12345-BRA-11-id-345";"A: Drug X";"A";"LOW";"WEEK 5 DAY 36";"52.52563791";"49.58468904"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 1 DAY 8";"63.55540711";"57.79381787"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 2 DAY 15";"63.55540711";"53.20778727"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 3 DAY 22";"63.55540711";"46.50804131"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 4 DAY 29";"63.55540711";"45.67316412"
      "AB12345-BRA-11-id-397";"A: Drug X";"B";"HIGH";"WEEK 5 DAY 36";"63.55540711";"39.65255343"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 1 DAY 8";"52.42786609";"44.08455627"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 2 DAY 15";"52.42786609";"52.69746487"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 3 DAY 22";"52.42786609";"58.63046275"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 4 DAY 29";"52.42786609";"59.66696007"
      "AB12345-BRA-11-id-50";"A: Drug X";"B";"LOW";"WEEK 5 DAY 36";"52.42786609";"48.81776863"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 1 DAY 8";"39.74591759";"54.46811519"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 2 DAY 15";"39.74591759";"58.56746406"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 3 DAY 22";"39.74591759";"62.44449616"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 4 DAY 29";"39.74591759";"49.58219322"
      "AB12345-BRA-11-id-9";"C: Combination";"B";"HIGH";"WEEK 5 DAY 36";"39.74591759";"31.86985923"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 1 DAY 8";"38.51253744";"49.78991235"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 2 DAY 15";"38.51253744";"52.15765632"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 3 DAY 22";"38.51253744";"52.77515777"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 4 DAY 29";"38.51253744";"46.59603872"
      "AB12345-BRA-12-id-59";"B: Placebo";"A";"HIGH";"WEEK 5 DAY 36";"38.51253744";"63.84339836"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"58.66051115";"58.02330884"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"58.66051115";"47.932631"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"58.66051115";"54.19017287"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"58.66051115";"38.77362987"
      "AB12345-BRA-13-id-177";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"58.66051115";"54.54441502"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 1 DAY 8";"56.1122029";"45.03807429"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 2 DAY 15";"56.1122029";"42.50657506"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 3 DAY 22";"56.1122029";"51.33632589"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 4 DAY 29";"56.1122029";"51.46558662"
      "AB12345-BRA-14-id-120";"C: Combination";"C";"HIGH";"WEEK 5 DAY 36";"56.1122029";"52.5780998"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 1 DAY 8";"53.90549966";"36.07474287"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 2 DAY 15";"53.90549966";"51.65371614"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 3 DAY 22";"53.90549966";"54.32860708"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 4 DAY 29";"53.90549966";"43.41184086"
      "AB12345-BRA-14-id-23";"A: Drug X";"B";"MEDIUM";"WEEK 5 DAY 36";"53.90549966";"43.37302674"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 1 DAY 8";"48.42133879";"50.57301306"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 2 DAY 15";"48.42133879";"41.05193138"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 3 DAY 22";"48.42133879";"59.90127632"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 4 DAY 29";"48.42133879";"51.72452253"
      "AB12345-BRA-15-id-36";"A: Drug X";"B";"HIGH";"WEEK 5 DAY 36";"48.42133879";"48.20972062"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 1 DAY 8";"57.91006483";"54.97906667"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 2 DAY 15";"57.91006483";"34.96128911"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 3 DAY 22";"57.91006483";"50.59813141"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 4 DAY 29";"57.91006483";"39.85104338"
      "AB12345-BRA-2-id-101";"B: Placebo";"A";"MEDIUM";"WEEK 5 DAY 36";"57.91006483";"47.74097512"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 1 DAY 8";"57.53687622";"38.7019598"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 2 DAY 15";"57.53687622";"44.97015184"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 3 DAY 22";"57.53687622";"57.65983342"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 4 DAY 29";"57.53687622";"52.13464322"
      "AB12345-BRA-2-id-296";"A: Drug X";"C";"HIGH";"WEEK 5 DAY 36";"57.53687622";"45.01484368"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 1 DAY 8";"38.61659204";"63.66251828"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 2 DAY 15";"38.61659204";"46.94717406"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 3 DAY 22";"38.61659204";"47.51358487"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 4 DAY 29";"38.61659204";"62.12765014"
      "AB12345-BRA-3-id-13";"B: Placebo";"A";"LOW";"WEEK 5 DAY 36";"38.61659204";"38.58603568"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 1 DAY 8";"50.4380212";"52.74254977"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 2 DAY 15";"50.4380212";"51.57955432"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 3 DAY 22";"50.4380212";"44.10220552"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 4 DAY 29";"50.4380212";"34.78926025"
      "AB12345-BRA-3-id-8";"B: Placebo";"B";"MEDIUM";"WEEK 5 DAY 36";"50.4380212";"42.40548755"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 1 DAY 8";"64.44871403";"40.15868367"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 2 DAY 15";"64.44871403";"56.10868726"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 3 DAY 22";"64.44871403";"38.43982358"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 4 DAY 29";"64.44871403";"39.85501425"
      "AB12345-BRA-4-id-368";"C: Combination";"B";"MEDIUM";"WEEK 5 DAY 36";"64.44871403";"54.90633766"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 1 DAY 8";"53.54191561";"50.9584111"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 2 DAY 15";"53.54191561";"42.24304843"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 3 DAY 22";"53.54191561";"45.89153143"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 4 DAY 29";"53.54191561";"37.14690379"
      "AB12345-BRA-4-id-383";"B: Placebo";"A";"HIGH";"WEEK 5 DAY 36";"53.54191561";"59.0242451"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 1 DAY 8";"46.40451953";"62.06415907"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 2 DAY 15";"46.40451953";"32.31162791"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 3 DAY 22";"46.40451953";"39.36419827"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 4 DAY 29";"46.40451953";"52.90701455"
      "AB12345-BRA-5-id-234";"C: Combination";"A";"MEDIUM";"WEEK 5 DAY 36";"46.40451953";"46.28288834"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 1 DAY 8";"50.12889161";"51.79801688"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 2 DAY 15";"50.12889161";"59.38716776"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 3 DAY 22";"50.12889161";"40.9292181"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 4 DAY 29";"50.12889161";"40.3866197"
      "AB12345-BRA-6-id-369";"A: Drug X";"A";"HIGH";"WEEK 5 DAY 36";"50.12889161";"48.07753903"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 1 DAY 8";"54.62606612";"54.50953768"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 2 DAY 15";"54.62606612";"43.28439046"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 3 DAY 22";"54.62606612";"60.32936162"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 4 DAY 29";"54.62606612";"53.05441096"
      "AB12345-BRA-7-id-301";"C: Combination";"B";"LOW";"WEEK 5 DAY 36";"54.62606612";"61.30877814"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 1 DAY 8";"43.08102992";"48.25184065"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 2 DAY 15";"43.08102992";"48.92231147"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 3 DAY 22";"43.08102992";"39.99679467"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 4 DAY 29";"43.08102992";"39.85133709"
      "AB12345-CAN-1-id-18";"A: Drug X";"A";"MEDIUM";"WEEK 5 DAY 36";"43.08102992";"48.33532464"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 1 DAY 8";"53.59961608";"53.09282171"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 2 DAY 15";"53.59961608";"43.8675434"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 3 DAY 22";"53.59961608";"49.56288108"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 4 DAY 29";"53.59961608";"58.37644522"
      "AB12345-CAN-1-id-341";"B: Placebo";"B";"MEDIUM";"WEEK 5 DAY 36";"53.59961608";"43.9545255"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 1 DAY 8";"46.49299033";"55.44430984"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 2 DAY 15";"46.49299033";"45.44546582"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 3 DAY 22";"46.49299033";"43.1332461"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 4 DAY 29";"46.49299033";"47.74561565"
      "AB12345-CAN-11-id-139";"A: Drug X";"B";"LOW";"WEEK 5 DAY 36";"46.49299033";"62.97058997"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 1 DAY 8";"50.62980745";"58.53109508"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 2 DAY 15";"50.62980745";"29.72895902"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 3 DAY 22";"50.62980745";"52.9082627"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 4 DAY 29";"50.62980745";"65.90462976"
      "AB12345-CAN-11-id-306";"C: Combination";"A";"HIGH";"WEEK 5 DAY 36";"50.62980745";"45.86928504"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 1 DAY 8";"37.86075699";"53.42297722"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 2 DAY 15";"37.86075699";"61.14149868"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 3 DAY 22";"37.86075699";"49.11191688"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 4 DAY 29";"37.86075699";"52.32121689"
      "AB12345-CAN-14-id-104";"A: Drug X";"B";"HIGH";"WEEK 5 DAY 36";"37.86075699";"42.79681252"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 1 DAY 8";"41.47118369";"39.3171842"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 2 DAY 15";"41.47118369";"45.80490508"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 3 DAY 22";"41.47118369";"37.75439439"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 4 DAY 29";"41.47118369";"52.40049525"
      "AB12345-CAN-4-id-324";"C: Combination";"B";"MEDIUM";"WEEK 5 DAY 36";"41.47118369";"51.04490632"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 1 DAY 8";"49.52881869";"37.13936423"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 2 DAY 15";"49.52881869";"43.60217652"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 3 DAY 22";"49.52881869";"57.25095293"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 4 DAY 29";"49.52881869";"52.75023299"
      "AB12345-CAN-4-id-331";"B: Placebo";"A";"MEDIUM";"WEEK 5 DAY 36";"49.52881869";"51.94990354"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 1 DAY 8";"56.08540439";"50.64515939"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 2 DAY 15";"56.08540439";"58.39085159"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 3 DAY 22";"56.08540439";"58.23111566"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 4 DAY 29";"56.08540439";"49.91943435"
      "AB12345-CAN-5-id-121";"C: Combination";"A";"HIGH";"WEEK 5 DAY 36";"56.08540439";"47.71210561"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 1 DAY 8";"58.03627562";"40.21813637"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 2 DAY 15";"58.03627562";"46.97678868"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 3 DAY 22";"58.03627562";"45.19739798"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 4 DAY 29";"58.03627562";"60.32665349"
      "AB12345-CHN-1-id-107";"B: Placebo";"B";"HIGH";"WEEK 5 DAY 36";"58.03627562";"45.84002151"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 1 DAY 8";"49.25720896";"52.79694364"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 2 DAY 15";"49.25720896";"46.97466787"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 3 DAY 22";"49.25720896";"44.79746183"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 4 DAY 29";"49.25720896";"42.11954487"
      "AB12345-CHN-1-id-119";"A: Drug X";"C";"MEDIUM";"WEEK 5 DAY 36";"49.25720896";"43.27039232"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 1 DAY 8";"60.17067693";"48.27665503"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 2 DAY 15";"60.17067693";"41.15831196"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 3 DAY 22";"60.17067693";"58.12504141"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 4 DAY 29";"60.17067693";"52.61436821"
      "AB12345-CHN-1-id-12";"B: Placebo";"C";"LOW";"WEEK 5 DAY 36";"60.17067693";"60.14166533"'
  )
}

get_mmrm <- function() {
  anl <- get_anl() %>%
    dplyr::mutate(
      ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")),
      AVISIT = factor(AVISIT)
    )

  mmrm <- fit_mmrm(
    vars = list(
      response = "AVAL",
      visit = "AVISIT",
      arm = "ARM",
      covariates = c("BMRKR2"),
      id = "USUBJID"
    ),
    data = anl,
    cor_struct = "unstructured"
  )
}

get_mmrm_no_arm <- function() {
  anl <- get_anl() %>%
    dplyr::mutate(
      ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")),
      AVISIT = factor(AVISIT)
    )

  mmrm <- fit_mmrm(
    vars = list(
      response = "AVAL",
      visit = "AVISIT",
      covariates = c("STRATA1"),
      id = "USUBJID"
    ),
    data = anl,
    cor_struct = "unstructured"
  )
}

# h_mmrm_fixed ----

test_that("h_mmrm_fixed works as expected", {
  mmrm <- get_mmrm()
  result <- expect_silent(h_mmrm_fixed(mmrm, format = "xx.xx"))
  expect_snapshot(result)
})

test_that("h_mmrm_cov works as expected", {
  mmrm <- get_mmrm()
  result <- expect_silent(h_mmrm_cov(mmrm, format = "xx.xx"))
  expect_snapshot(result)
})

test_that("h_mmrm_diagnostic works as expected", {
  mmrm <- get_mmrm()
  result <- expect_silent(h_mmrm_diagnostic(mmrm, format = "xx.x"))
  expect_snapshot(result)
})

# tidy.mmrm ----

test_that("tidy.mmrm works as expected", {
  mmrm <- get_mmrm()
  result <- broom::tidy(mmrm)
  expect_data_frame(result, nrows = 15L, ncols = 17L)
  expect_named(
    result,
    c(
      "ARM", "AVISIT", "estimate_est", "se_est", "df_est", "lower_cl_est",
      "upper_cl_est", "n", "estimate_contr", "se_contr", "df_contr",
      "lower_cl_contr", "upper_cl_contr", "t_stat", "p_value", "relative_reduc",
      "conf_level"
    )
  )
})

test_that("tidy.mmrm works as expected when treatment is not considered in the model", {
  mmrm <- get_mmrm_no_arm()
  result <- broom::tidy(mmrm)
  expect_data_frame(result, nrows = 5L, ncols = 8L)
  expect_named(
    result,
    c(
      "AVISIT", "estimate_est", "se_est", "df_est", "lower_cl_est",
      "upper_cl_est", "n", "conf_level"
    )
  )
})

# s_mmrm_lsmeans ----

test_that("s_mmrm_lsmeans works as expected when not in reference column", {
  mmrm <- get_mmrm()
  df <- broom::tidy(mmrm)
  result <- s_mmrm_lsmeans(df[8, ], FALSE)
  expect_list(result, names = "unique")
  expect_named(result, c("n", "adj_mean_se", "adj_mean_ci", "diff_mean_se", "diff_mean_ci", "change", "p_value"))
  expect_identical(result$n, 17L)
  expect_numeric(result$adj_mean_se, len = 2L, any.missing = FALSE)
  expect_numeric(result$adj_mean_ci, len = 2L, any.missing = FALSE)
  expect_identical(attr(result$adj_mean_ci, "label"), "95% CI")
  expect_numeric(result$diff_mean_se, len = 2L, any.missing = FALSE)
  expect_numeric(result$diff_mean_ci, len = 2L, any.missing = FALSE)
  expect_identical(attr(result$diff_mean_ci, "label"), "95% CI")
  expect_number(result$change)
  expect_identical(attr(result$change, "label"), "Relative Reduction (%)")
  expect_number(result$p_value)
})

test_that("s_mmrm_lsmeans works as expected when in reference column", {
  mmrm <- get_mmrm()
  df <- broom::tidy(mmrm)
  result <- s_mmrm_lsmeans(df[2, ], TRUE)
  expect_list(result, names = "unique")
  expect_named(result, c("n", "adj_mean_se", "adj_mean_ci", "diff_mean_se", "diff_mean_ci", "change", "p_value"))
  expect_identical(result$n, 11L)
  expect_numeric(result$adj_mean_se, len = 2L, any.missing = FALSE)
  expect_numeric(result$adj_mean_ci, len = 2L, any.missing = FALSE)
  expect_identical(attr(result$adj_mean_ci, "label"), "95% CI")
  expect_identical(result$diff_mean_se, character(0))
  expect_identical(result$diff_mean_ci, formatters::with_label(character(0), label = "95% CI"))
  expect_identical(result$change, formatters::with_label(character(0), label = "Relative Reduction (%)"))
  expect_identical(result$p_value, character(0))
})

test_that("s_mmrm_lsmeans_single works as expected", {
  mmrm <- get_mmrm_no_arm()
  df <- broom::tidy(mmrm)
  result <- s_mmrm_lsmeans_single(df[2, ])
  expected <- list(
    n = 41L,
    adj_mean_se = c(48.7, 1.19),
    adj_mean_ci = formatters::with_label(c(46.3, 51.1), label = "95% CI")
  )
  expect_equal(result, expected, tolerance = 1e-2)
})

# summarize_lsmeans ----

test_that("summarize_lsmeans works as expected", {
  mmrm <- get_mmrm()
  df <- broom::tidy(mmrm)
  result <- basic_table() %>%
    split_cols_by("ARM", ref_group = mmrm$ref_level) %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(
      show_relative = "increase",
      .formats = c(
        n = "xx.",
        adj_mean_se = sprintf_format("%.1f (%.1f)"),
        adj_mean_ci = "(xx.x, xx.x)",
        diff_mean_se = sprintf_format("%.1f (%.1f)"),
        diff_mean_ci = "(xx.x, xx.x)",
        change = "xx.%",
        p_value = "xx.xx"
      )
    ) %>%
    build_table(df)
  expect_snapshot(result)
})

test_that("summarize_lsmeans works as expected when treatment is not considered in the model", {
  mmrm <- get_mmrm_no_arm()
  df <- broom::tidy(mmrm)
  result <- basic_table() %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(
      arms = FALSE,
      .formats = c(
        n = "xx.",
        adj_mean_se = sprintf_format("%.1f (%.1f)"),
        adj_mean_ci = "(xx.x, xx.x)"
      )
    ) %>%
    build_table(df)
  expect_snapshot(result)
})

test_that("summarize_lsmeans works with averages of visits as expected", {
  anl <- get_anl() %>%
    dplyr::mutate(
      ARM = factor(ARM, levels = c("B: Placebo", "A: Drug X", "C: Combination")),
      AVISIT = factor(AVISIT)
    )

  mmrm <- fit_mmrm(
    vars = list(
      response = "AVAL",
      visit = "AVISIT",
      arm = "ARM",
      covariates = c("BMRKR2"),
      id = "USUBJID"
    ),
    data = anl,
    averages_emmeans = list(
      "W1D8 + W2D15" = c("WEEK 1 DAY 8", "WEEK 2 DAY 15"),
      "W3D22 + W5D36" = c("WEEK 3 DAY 22", "WEEK 5 DAY 36")
    ),
    cor_struct = "unstructured"
  )

  df <- broom::tidy(mmrm)
  result <- basic_table() %>%
    split_cols_by("ARM", ref_group = mmrm$ref_level) %>%
    split_rows_by("AVISIT") %>%
    summarize_lsmeans(
      .formats = c(
        n = "xx.",
        adj_mean_se = sprintf_format("%.1f (%.1f)"),
        adj_mean_ci = "(xx.x, xx.x)",
        diff_mean_se = sprintf_format("%.1f (%.1f)"),
        diff_mean_ci = "(xx.x, xx.x)",
        change = "xx.%",
        p_value = "xx.xx"
      )
    ) %>%
    build_table(df)
  expect_snapshot(result)
})
