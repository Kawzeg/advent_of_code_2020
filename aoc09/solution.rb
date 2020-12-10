xs = File.open("input").read.split("\n").map(&:to_i)

preamble = 25

target_index = (preamble..).detect{ |i|
  !xs[i-preamble..i].combination(2).map(&:sum).include? xs[i]
}
p xs[target_index]
p (2..target_index).each{ |s|
  j = (0..target_index).detect{|k|xs[k,s].sum==xs[target_index]}
  break xs[j,s].min + xs[j,s].max if j
}

# Slightly golfed version
xs = File.open("input").read.split("\n").map(&:to_i)
p xs[i=(25..).detect{|i|!xs[i-25,25].combination(2).map(&:sum).include? xs[i]}]
p (2..i).each{|j|k=(0..i).detect{|k|xs[k,j].sum==xs[i]}
  break xs[k,j].min+xs[k,j].max if k}
