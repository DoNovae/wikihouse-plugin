require_relative "hershey/character"
require_relative "hershey/document"
require_relative "hershey/font"
require_relative "hershey/page"
require_relative "hershey/version"
require_relative "hershey/word"

module Hershey
  def self.new(options = {})
    doc = Document.new(options)
  end

  def self.svg(text, options = {})
    doc = Document.new(options)
    doc.write(text)
    doc.svg
  end

  def self.svgs(text, options = {})
    puts options
    doc = Document.new(options)
    doc.write(text)
    doc.svgs
  end
end
