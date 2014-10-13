=begin


=end

require 'pathname'

dir = "/Users/slevin/wrk/words"
count = 0
Dir.foreach(dir) do |filename|
  p = (Pathname.new dir) + filename
  if (p.file?)
    f = p.open
    s = f.read
    num = s.split.length
    count += num
  end
end

puts count / 250
