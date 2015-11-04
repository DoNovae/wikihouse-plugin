module Hershey
  class Character
    OFFSET = ' '.ord

    def initialize(character, font: :futural)
      @char = character.to_s[0] || ' '
      @font = font
      @match = FONTS[@font][@char.ord - OFFSET].match(/\A(?<midpoint>[\d-]+) +(?<width>[\d-]+) ?(?<path>[\w -]*)\Z/)
      @path = @match['path']
      @midpoint = @match['midpoint'].to_i
      @add_offset = @match['width'].to_i - @midpoint
    end

    def to_path(offset)
      %Q{<path stroke="red" fill="none" d="#{@path}" transform="translate(#{offset - @midpoint},0)"></path>}
    end

    def spacing
      @add_offset
    end

    def char
      @char
    end

    alias_method :to_s, :char
  end
end
