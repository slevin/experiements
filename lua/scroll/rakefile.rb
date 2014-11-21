task :test do
  sh 'busted -v -p _test test'
end

task :term do
  sh '"/Applications/CoronaSDK/Corona Terminal" ./main.lua'
end