module Hershey
  class Document
    BUFFER = 20.freeze
    SIDE = 10.freeze
    HEIGHT_STRING = "jfdsklahfdjksjhaljfdskahj".freeze

    def initialize(width: 1000, height: 1300, spacing: 3, vertical: 20, font: :futural, stroke: 1)
      @words = []
      @pages = []
      @width = width
      @height = height
      @spacing = spacing
      @line = BUFFER
      @vertical = vertical
      @closed = false
      @font = font
      @stroke = stroke
    end

    def [](page)
      @pages[page].svg
    end

    def length
      @pages.length
    end

    def <<(text)
      word = ""
      text.each_char do |c|
        if c == " "
          @words << Word.new(word, font: @font)
          word = ''
          @words << :space
        elsif c == "\n"
          @words << Word.new(word, font: @font)
          word = ''
          @words << :break
        else
          word << c
        end
      end
      @words << Word.new(word, font: @font) unless word == ''
    end

    alias_method :write, :<<

    def svg
      write_out unless @closed
      self[0]
    end

    def svgs
      write_out unless @closed
      @pages.map {|page| page.svg}
    end

    private

    def write_out
      new_page
      @words.each do |word|
        begin
          @pages.last.write(word)
        rescue PageFullError => e
          new_page
          retry
        end
      end
    end

    def new_page
      @pages << Page.new(width: @width, height: @height, stroke: @stroke)
    end
  end
end
