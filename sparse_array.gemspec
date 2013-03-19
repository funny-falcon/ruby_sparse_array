# coding: utf-8
lib = File.expand_path('../lib', __FILE__)
$LOAD_PATH.unshift(lib) unless $LOAD_PATH.include?(lib)
require 'sparse_array/version'

Gem::Specification.new do |spec|
  spec.name          = "sparse_array"
  spec.version       = SparseArray::VERSION
  spec.authors       = ["Sokolov Yura aka funny_falcon"]
  spec.email         = ["funny.falcon@gmail.com"]
  spec.description   = %q{Sparse Array - map from integers (0..2**32-1) to objects}
  spec.summary       = %q{lightweight map from integers to objects}
  spec.homepage      = "https://github.com/funny-falcon/ruby_sparse_array"
  spec.license       = "MIT"

  spec.files         = %w{ext/extconf.rb ext/sp_ar.c lib/sparse_array/version.rb Gemfile LICENSE.txt sparse_array.gemspec}
  spec.extensions    = ["ext/extconf.rb"]
  spec.require_paths = ["lib", "ext"]

  spec.add_development_dependency "bundler", "~> 1.3"
  spec.add_development_dependency "rake"
end
