# SparseArray

Simple hash like map from integer (0..(2^32-1)) to objects

## Installation

Add this line to your application's Gemfile:

    gem 'sparse_array'

And then execute:

    $ bundle

Or install it yourself as:

    $ gem install sparse_array

## Usage

    a = SparseArray.new
    a[1] = 9
    a[5] = a.fetch(5, 0) + 1
    a.delete(1)
    puts a.inspect
    a.each.to_a

Methods defined: 

````
    a[key]
    a[key]=val
    a.fetch(key, default)
    a.delete(key)
    a.clear
    a.empty?
    a.size
    a.count
    a.include?
    a.has_key?
    a.keys
    a.values
    a.each{}
    a.each_pair{}
    a.each_key{}
    a.each_value{}
    a.inspect
    a.initialize_copy(copy)
    include Enumerable
````
   

## Contributing

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
