task :test do
  sh 'busted -p _test test'
end

task :term do
  sh '"/Applications/CoronaSDK/Corona Terminal"'
end