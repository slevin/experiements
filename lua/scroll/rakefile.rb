task :test do
  sh 'busted -p _test test'
end

task :term do
  sh '"/Applications/CoronaSDK/Corona Simulator.app/Contents/MacOS/Corona Simulator" -no-console YES ./main.lua'
end