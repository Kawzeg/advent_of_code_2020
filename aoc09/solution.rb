read = File.open("input").read

preamble = 25

def sums(q)
  q.flat_map{|x| q.flat_map{|y| y != x ? [x+y] : []}}
end

q = []
p read.split("\n").map{|x|x.to_i}.each{|x|
  if q.length == preamble
    if !sums(q).include? x
      break x
    end
    q.pop()
  end
  q.prepend(x)
}
