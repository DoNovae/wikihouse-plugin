module Hershey
  class Page
    SPACE = Character.new(' ').spacing.freeze
    BUFFER = 20.freeze
    SIDE = 10.freeze
    HEIGHT_STRING = "jfdsklahfdjksjhaljfdskahj".freeze

    def initialize(width: 1000, height: 1300, stroke: 1)
      @close = false
      @height = height
      @width = width
      @current_offset = SIDE
      @line = BUFFER
      @svg = <<-HEADER
<?xml version="1.0" standalone="no"?>
<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN"
  "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
<svg width="#{@width}" height="#{HEIGHT_STRING}" version="1.1"
  xmlns="http://www.w3.org/2000/svg" style="stroke-width:#{stroke};">
      HEADER
      @svg << %Q{<g transform="translate(#{SIDE},#{@line})">}
    end

    def <<(word)
      if word.is_a?(Word)
        if word.spacing + @current_offset > @width
          new_line
        end
        @svg << word.to_path(@current_offset)
        @current_offset += word.spacing
      elsif word == :space
        @current_offset += SPACE
      elsif word == :break
        new_line
      end
    end
    alias_method :write, :<<

    def write_out
      close
      @svg
    end
    alias_method :svg, :write_out

    private

    def new_line
      @line += BUFFER * 2
      if @line > @height
        @line -= BUFFER
        raise PageFullError, "The page is full"
      else
        @current_offset = SIDE
        @svg << %Q{</g><g transform="translate(#{SIDE},#{@line})">}
      end
    end

    def close
      unless @close
        @svg << "</g></svg>"
        @svg.gsub!(HEIGHT_STRING, (@line + BUFFER).to_s)
        @close = true
      end
    end
  end

  class PageFullError < StandardError
  end
end