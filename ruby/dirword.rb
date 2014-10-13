=begin


=end

require 'pathname'

dir = "/Users/slevin/wrk/words"
count = 0
Dir.foreach(dir) do |filename|
  p = (Pathname.new dir) + filename
  if (p.file?)
    count += p.open.read.split.length
  end
end

puts count / 250
