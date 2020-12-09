xs = File.open("input").read.split("\n").map(&:to_i)

preamble = 25

target_index = (preamble..xs.size).each{ |i|
  break i if !xs[i-preamble..i].combination(2).map(&:sum).include? xs[i]
}
p xs[target_index]
p target_index.times.each{ |i|
  sums = target_index.times.map{|j| xs[j,i]}.map(&:sum)
  j = sums.find_index xs[target_index]
  break xs[j,i].min + xs[j,i].max if j
}
