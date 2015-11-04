# Public Domain (-) 2011 The WikiHouse Authors.
# See the WikiHouse UNLICENSE file for details.

# =========================
# WikiHouse SketchUp Plugin
# =========================

require 'sketchup.rb'
require File.join('wikihouse', 'JSON.rb')
require File.join('wikihouse', 'WebDialog.rb')
require File.join('wikihouse', 'hershey.rb')
module WikiHouseExtension
# ------------------------------------------------------------------------------
# Path Utilities
# ------------------------------------------------------------------------------

def self.get_documents_directory(home, docs)
  dir = File.join home, docs
  if not (File.directory?(dir) and File.writable?(dir))
    home
  else
    dir
  end
end

def self.get_temp_directory
  temp = '.'
  for dir in [ENV['TMPDIR'], ENV['TMP'], ENV['TEMP'], ENV['USERPROFILE'], '/tmp']
	if dir and File.directory?(dir) and File.writable?(dir)
	  temp = dir
	  break
	end
  end
  File.expand_path temp
end

# ------------------------------------------------------------------------------
# Some Constants
# ------------------------------------------------------------------------------
LAYER_INNER = "inner"
LAYER_OUTER = "outer"
LAYER0 = "Layer0"
PANEL_ID_ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
PANEL_ID_ALPHABET_LENGTH = PANEL_ID_ALPHABET.length

REPLY_ABORT = 3
REPLY_CANCEL = 2
REPLY_NO = 7
REPLY_OK = 1
REPLY_RETRY = 4
REPLY_YES = 6

#if RUBY_PLATFORM =~ /mswin/
if RUBY_PLATFORM == 'x64-mingw32'
  WIKIHOUSE_CONF_FILE = File.join ENV['APPDATA'], 'WikiHouse.conf'
  WIKIHOUSE_SAVE = get_documents_directory ENV['USERPROFILE'], 'Documents'
  WIKIHOUSE_MAC = false
else
  WIKIHOUSE_CONF_FILE = File.join ENV['HOME'], '.wikihouse.conf'
  WIKIHOUSE_SAVE = get_documents_directory ENV['HOME'], 'Documents'
  WIKIHOUSE_MAC = true
end

WIKIHOUSE_DEV = false
WIKIHOUSE_HIDE = false
WIKIHOUSE_LOCAL = false
WIKIHOUSE_SHORT_CIRCUIT = false

WEBDIALOG_PATH=File.join File.dirname(__FILE__), "wikihouse/webdialog"

WIKIHOUSE_PLUGIN_VERSION = "0.21"
WIKIHOUSE_SPEC = "0.1"
WIKIHOUSE_TEMP = get_temp_directory
WIKIHOUSE_TITLE = "WikiHouse"

WIKIHOUSE_SCALE = (1.inch).to_mm
WIKIHOUSE_THICKNESS = 18.mm
#WIKIHOUSE_THICKNESS = 12.mm
WIKIHOUSE_FONT_HEIGHT = 30.mm
WIKIHOUSE_PANEL_PADDING = (25.0.mm / 2.0)
WIKIHOUSE_SHEET_HEIGHT = 1200.mm
WIKIHOUSE_SHEET_MARGIN = 17.5.mm - WIKIHOUSE_PANEL_PADDING
WIKIHOUSE_SHEET_WIDTH = 2400.mm
WIKIHOUSE_DRILL_WIDTH = 3.mm
WIKIHOUSE_LAYOUT_SCALE = 1

WIKIHOUSE_SHEET_INNER_HEIGHT = WIKIHOUSE_SHEET_HEIGHT - (2 * WIKIHOUSE_SHEET_MARGIN)
WIKIHOUSE_SHEET_INNER_WIDTH = WIKIHOUSE_SHEET_WIDTH - (2 * WIKIHOUSE_SHEET_MARGIN)

  
  # ----------------------------------------------------------------------------
  # Settings
  # ----------------------------------------------------------------------------
  # Set WikiHouse Panel Dimensions
  wikihouse_sheet_height  = WIKIHOUSE_SHEET_HEIGHT
  wikihouse_sheet_width   = WIKIHOUSE_SHEET_WIDTH
  wikihouse_sheet_depth   = WIKIHOUSE_THICKNESS
  wikihouse_panel_padding = WIKIHOUSE_PANEL_PADDING
  wikihouse_sheet_margin  = WIKIHOUSE_SHEET_MARGIN
  wikihouse_font_height   = WIKIHOUSE_FONT_HEIGHT
  wikihouse_sheet_inner_height = WIKIHOUSE_SHEET_INNER_HEIGHT
  wikihouse_sheet_inner_width  = WIKIHOUSE_SHEET_INNER_WIDTH
  wikihouse_drill_width = WIKIHOUSE_DRILL_WIDTH
  wikihouse_scale = WIKIHOUSE_LAYOUT_SCALE
  # Store the actual values as length objects (in inches)
  @settings = {
    'sheet_height'       => wikihouse_sheet_height.to_inch,
    'sheet_inner_height' => wikihouse_sheet_inner_height.to_inch,
    'sheet_width'        => wikihouse_sheet_width.to_inch, 
    'sheet_inner_width'  => wikihouse_sheet_inner_width.to_inch,
    'sheet_depth'        => wikihouse_sheet_depth.to_inch, 
    'padding'            => wikihouse_panel_padding,
    'margin'             => wikihouse_sheet_margin.to_inch,
    'font_height'        => wikihouse_font_height.to_inch,
    'drill_width'        => wikihouse_drill_width.to_inch,
    'scale'              => wikihouse_scale,
  }


  
  class << self
    attr_accessor :settings
  end
  
  def self.dimensions_mm()
       sheet_height,sheet_width,sheet_inner_height,sheet_inner_width,margin,padding,font_height,drill_width=WikiHouseExtension.dimensions_inch()
  	  return [
  	  sheet_height.to_mm,
  	  sheet_width.to_mm,
  	  sheet_inner_height.to_mm,
  	  sheet_inner_width.to_mm,
  	  margin.to_mm,
  	  padding.to_mm,
  	  font_height.to_mm,
  	  drill_width.to_mm
  	  ]
  end
  
    def self.dimensions_inch()
  	  return [
  	  settings["sheet_height"],
  	  settings["sheet_width"],
  	  settings["sheet_height"]-2*settings["margin"],
  	  settings["sheet_width"]-2*settings["margin"],
  	  settings["margin"],
  	  settings["padding"],
  	  settings["font_height"],
  	  settings["drill_width"]
  	  ]
  end
    
  @back_material
  @front_material
    class << self
    attr_accessor :back_material
  end
        class << self
    attr_accessor :front_material
  end
  # Store default values for resetting.
  DEFAULT_SETTINGS = Hash[@settings]
# ------------------------------------------------------------------------------
# Utility Functions
# ------------------------------------------------------------------------------

def self.gen_status_msg(msg)
  return [
    msg + " .",
    msg + " ..",
    msg + " ...",
    msg + " ....",
    msg + " .....",
  ]
end

def self.get_wikihouse_thumbnail(model, view, suffix)
  filename = File.join WIKIHOUSE_TEMP, "#{model.guid}-#{suffix}.png"
  opts = {
    :antialias => true,
    :compression => 0.8,
    :filename => filename,
    :height => [view.vpheight, 1600].min,
    :transparent => true,
    :width => [view.vpwidth, 1600].min
  }
  view.write_image opts
  data = File.open(filename, 'rb') do |io|
    io.read
  end
  File.delete filename
  data
end

def self.set_dom_value(dialog, id, value)
  if value.length > 2097152
    dialog.execute_script "WIKIHOUSE_DATA = [#{value[0...2097152].inspect}];"
    start, stop = 2097152, (2097152+2097152)
    idx = 1
    while 1
      segment = value[start...stop]
      if not segment
        break
      end
      dialog.execute_script "WIKIHOUSE_DATA[#{idx}] = #{segment.inspect};"
      idx += 1
      start = stop
      stop = stop + 2097152
    end
    dialog.execute_script "document.getElementById('#{id}').value = WIKIHOUSE_DATA.join('');"
  else
    dialog.execute_script "document.getElementById('#{id}').value = #{value.inspect};"
  end
end

def show_wikihouse_error(msg)
  UI.messagebox "!! ERROR !!\n\n#{msg}"
end

# ------------------------------------------------------------------------------
# Centroid Calculation
# ------------------------------------------------------------------------------

def self.get_face_center(face)

  # First, triangulate the polygon.
  mesh = face.mesh

  # Initialise aggregation variables.
  idx = 0
  xs = []
  ys = []
  areas = []

  # For each triangle, calculate the surface area and center of mass.
  for i in 0...mesh.count_polygons

    a, b, c = mesh.polygon_points_at i+1

    ax, ay, _ = a.to_a
    bx, by, _ = b.to_a
    cx, cy, _ = c.to_a

    dax = ax - bx
    dbx = bx - cx
    dcx = cx - ax
    day = ay - by
    dby = by - cy
    dcy = cy - ay

    la = Math.sqrt((dax * dax) + (day * day))
    lb = Math.sqrt((dbx * dbx) + (dby * dby))
    lc = Math.sqrt((dcx * dcx) + (dcy * dcy))

    max = (ax + bx) / 2
    mbx = (bx + cx) / 2
    mcx = (cx + ax) / 2
    may = (ay + by) / 2
    mby = (by + cy) / 2
    mcy = (cy + ay) / 2

    px = ((max * la) + (mbx * lb) + (mcx * lc)) / (la + lb + lc)
    py = ((may * la) + (mby * lb) + (mcy * lc)) / (la + lb + lc)

    # angle = (Math.acos((la * la) + (lb * lb) - (lc * lc)) * Math::PI) / (360 * la * lb)
    # area = (la * lb * Math.sin(angle)) / 2

    s1, s2, s3 = [la, lb, lc].sort.reverse
    top = (s1 + (s2 + s3)) * (s3 - (s1 - s2)) * (s3 + (s1 - s2)) * (s1 + (s2 - s3))

    # TODO(tav): Read http://www.eecs.berkeley.edu/~wkahan/Triangle.pdf and
    # figure out why this fails on triangles with small angles.
    if top < 0
      puts "Failed surface area calculation"
      next
    end

    area = Math.sqrt(top) / 4

    xs[idx] = px
    ys[idx] = py
    areas[idx] = area

    idx += 1

  end

  # Calculate the total surface area.
  total = areas.inject(0) { |t, a| a + t }

  # Calculate the weighted center points.
  px, py = 0, 0
  for i in 0...xs.length
    x, y, a = xs[i], ys[i], areas[i]
    px += x * a
    py += y * a
  end

  # Calculate the center of mass.
  px = px / total
  py = py / total

  [px, py]

end

# ------------------------------------------------------------------------------
# Status Messages
# ------------------------------------------------------------------------------

WIKIHOUSE_DETECTION_STATUS = gen_status_msg "Detecting matching faces"
WIKIHOUSE_DXF_STATUS = gen_status_msg "Generating DXF output"
WIKIHOUSE_LAYOUT_STATUS = gen_status_msg "Nesting panels for layout"
WIKIHOUSE_PANEL_STATUS = gen_status_msg "Generating panel data"
WIKIHOUSE_SVG_STATUS = gen_status_msg "Generating SVG output"

# ------------------------------------------------------------------------------
# Load Handler
# ------------------------------------------------------------------------------

class WikiHouseLoader

  attr_accessor :cancel, :error

  def initialize(name)
    @cancel = false
    @error = nil
    @name = name
  end

  def cancelled?
    @cancel
  end

  def onFailure(error)
    @error = error
    Sketchup.set_status_text ''
  end

  def onPercentChange(p)
    Sketchup.set_status_text "LOADING #{name}:    #{p.to_i}%"
  end

  def onSuccess
    Sketchup.set_status_text ''
  end

end

# ------------------------------------------------------------------------------
# App Observer
# ------------------------------------------------------------------------------

class WikiHouseAppObserver < Sketchup::AppObserver

  def onNewModel(model)
  end

  # TODO(tav): This doesn't seem to be getting called.
  def onQuit
    if WIKIHOUSE_DOWNLOADS.length > 0
      show_wikihouse_error "Aborting downloads from #{WIKIHOUSE_TITLE}"
    end
    if WIKIHOUSE_UPLOADS.length > 0
      show_wikihouse_error "Aborting uploads to #{WIKIHOUSE_TITLE}"
    end
  end

end

# ------------------------------------------------------------------------------
# Dummy Group
# ------------------------------------------------------------------------------

class WikiHouseDummyGroup

  attr_reader :name

  def initialize
    @name = "Ungrouped Objects"
  end

end

WIKIHOUSE_DUMMY_GROUP = WikiHouseDummyGroup.new

# ------------------------------------------------------------------------------
# DXF Writer
# ------------------------------------------------------------------------------

class WikiHouseDXF

  def initialize(layout)
  end

  def generate
    ""
  end

end

# ------------------------------------------------------------------------------
# SVG Writer
# ------------------------------------------------------------------------------

class WikiHouseSVG

  def initialize(layout, scale)
    @layout = layout
    @scale = scale
  end

  def generate
    layout = @layout
    scale = @scale

    sheet_height, sheet_width, inner_height, inner_width, margin,padding,font_height,drill_width = layout.dimensions
    sheets = layout.sheets
    count = sheets.length
    drill_width=0.04.mm.to_inch if drill_width==0

    scaled_height = scale * sheet_height
    scaled_width = scale * sheet_width
    total_height = scale * ((count * (sheet_height + (12 * margin))) + (margin * 10))
    total_width = scale * (sheet_width + (margin * 2))
    font_height= scale * font_height
    drill_width= scale * drill_width

    svg = []
    svg << <<-HEADER.gsub(/^ {6}/, '')
      <?xml version="1.0" standalone="no"?>
      <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">
      <svg height="#{total_height}mm" width="#{total_width}mm" version="1.1" units="mm"
           viewBox="0 0 #{total_width} #{total_height}" xmlns="http://www.w3.org/2000/svg"
           xmlns:xlink="http://www.w3.org/1999/xlink" style="background-color: #ffffff;">
      <desc>#{WIKIHOUSE_TITLE} Cutting Sheets</desc>"
      <!-- linkstart -->
      <g visibility="hidden" pointer-events="all">
        <rect x="0" y="0" width="100%" height="100%" fill="none" />
      </g>
      HEADER

    loop_count = 0

    for s in 0...count

      sheet = sheets[s]
      base_x = scale * margin
      base_y = scale * ((s * (sheet_height + (12 * margin))) + (margin * 9))

      svg << "<rect x=\"#{base_x}\" y=\"#{base_y}\" width=\"#{scaled_width}\" height=\"#{scaled_height}\" fill=\"none\" stroke=\"rgb(210, 210, 210)\" stroke-width=\"1\" />"

      base_x += scale * margin
      base_y += scale * margin

      sheet.each do |loops, circles, outer_mapped, centroid, label|

        Sketchup.set_status_text WIKIHOUSE_SVG_STATUS[(loop_count/5) % 5]
        loop_count += 1

        svg << '<g fill="none" stroke="rgb(255, 255, 255)" stroke-width="#{drill_width}" >'
        
        if label and label != ""
          #svg << <<-LABEL.gsub(/^ {12}/, '')
            #<text x="#{(scale * centroid.x) + base_x}" y="#{(scale * centroid.y) + base_y}" style="font-size: #{font_height}; stroke: none; fill: rgb(255, 0, 0); text-family: monospace">#{label}</text>
            #LABEL
            word=Hershey::Word.new(label, font: :futural )
            svg<< word.to_path(font_height/30,(scale * centroid.x) + base_x,(scale * centroid.y) + base_y)
        end
        
        for i in 0...loops.length
          circle = circles[i]
          if circle
            center, radius = circle
            x = (scale * center.x) + base_x
            y = (scale * center.y) + base_y
            radius = scale * radius
            svg << <<-CIRCLE.gsub(/^ {14}/, '')
              <circle cx="#{x}mm" cy="#{y}mm" r="#{radius}"
                      stroke="rgb(51, 51, 51)" stroke-width="#{drill_width}" fill="none"  />
              CIRCLE
          else
            loop = loops[i]
            first = loop.shift
            path = []
            path << "M #{(scale * first.x) + base_x} #{(scale * first.y) + base_y}"
            loop.each do |point|
              path << "L #{(scale * point.x) + base_x} #{(scale * point.y) + base_y}"
            end
            path << "Z"
            svg << <<-PATH.gsub(/^ {14}/, '')
              <path d="#{path.join ' '}" stroke="rgb(0, 0, 0)" stroke-width="#{drill_width}" fill="none" />"
              PATH
          end
        end

        svg << '</g>'

      end
    end

    svg << '<!-- linkend -->'
    svg << '</svg>'
    svg.join "\n"

  end

end

# ------------------------------------------------------------------------------
# Layout Engine "
# ------------------------------------------------------------------------------

class WikiHouseLayoutEngine

  attr_accessor :sheets
  attr_reader :dimensions

  def initialize(panels, root, dimensions)

    @dimensions = dimensions
    @sheets = sheets = []

    # Set local variables to save repeated lookups. INCH
    sheet_height, sheet_width, inner_height, inner_width,
    sheet_margin, panel_padding, font_height, drill_width = dimensions

    # Filter out the singletons from the other panels.
    singletons = panels.select { |panel| panel.singleton }
    panels = panels.select { |panel| !panel.singleton }

    # Loop through the panels.
    panels.map! do |panel|

      # Get padding related info.
      no_padding = panel.no_padding

      # Get the bounding box.
      min = panel.min
      max = panel.max
      min_x, min_y = min.x, min.y
      max_x, max_y = max.x, max.y

      # Set a flag to indicate clipped panels.
      clipped = false

      # Determine if the potential savings exceeds the hard-coded threshold. If
      # so, see if we can generate an outline with rectangular areas clipped
      # from each corner.
      if (panel.bounds_area - panel.shell_area) > 50
        # puts (panel.bounds_area - panel.shell_area)
      end

      # Otherwise, treat the bounding box as the outline.
      if not clipped

        # Define the inner outline.
        inner = [[min_x, min_y, 0], [max_x, min_y, 0], [max_x, max_y, 0], [min_x, max_y, 0]]

        # Add padding around each side.
        if not no_padding
          min_x -= panel_padding
          min_y -= panel_padding
          max_x += panel_padding
          max_y += panel_padding
        elsif no_padding == "w"
          min_y -= panel_padding
          max_y += panel_padding
        elsif no_padding == "h"
          min_x -= panel_padding
          max_x += panel_padding
        end

        # Calculate the surface area that will be occupied by this panel.
        width = max_x - min_x
        height = max_y - min_y
        area = width * height

        # Define the padded outer outline.
        # outline = [[min_x, max_y, 0], [max_x, max_y, 0], [max_x, min_y, 0], [min_x, min_y, 0]]
        outer = [[min_x, min_y, 0], [max_x, min_y, 0], [max_x, max_y, 0], [min_x, max_y, 0]]
        outlines = [[nil, inner, outer]]

        # See if the panel can be rotated, if so add the transformation.
        if not no_padding
          if (inner_width > height) and (inner_height > width)
            # inner = [inner[3], inner[0], inner[1], inner[2]]
            # outer = [outer[3], outer[0], outer[1], outer[2]]
            outlines << [90.degrees, inner, outer]
            outlines << [270.degrees, inner, outer]
          end
          outlines << [180.degrees, inner, outer]
        end

      end

      # Save the generated data.
      [panel, outlines, area, panel.labels.dup]

    end

    # Sort the panels by surface area.
    panels = panels.sort_by { |data| data[2] }.reverse

    # Generate new groups to hold sheet faces.
    inner_group = root.add_group
    inner_faces = inner_group.entities
    outer_group = root.add_group
    outer_faces = outer_group.entities
    temp_group = root.add_group
    temp_faces = temp_group.entities
    total_area = inner_width * inner_height

    # Initialise the loop counter.
    loop_count = 0

    # Make local certain global constants.
    outside = Sketchup::Face::PointOutside

    # panels = panels[-10...-1]
    # panels = panels[-5...-1]
    c = 0

    # Do the optimising layout.
    while 1

      # Create a fresh sheet.
      sheet = []
      available_area = total_area
      idx = 0
      placed_i = []
      placed_o = []

      while available_area > 0

        Sketchup.set_status_text WIKIHOUSE_LAYOUT_STATUS[(loop_count/20) % 5]
        loop_count += 1

        panel_data = panels[idx]
        if not panel_data
          break
        end

        panel, outlines, panel_area, labels = panel_data
        if panel_area > available_area
          idx += 1
          next
        end

        match = true
        t = nil
        used = nil

        # If this is the first item, do the cheap placement check.
        if sheet.length == 0
          transform, inner, outer = outlines[0]
          point = outer[0]
          translate = Geom::Transformation.translation [-point[0], -point[1], 0]
          inner.each do |point|
            point = translate * point
            if (point.x > inner_width) or (-point.y > inner_height)
              p (point.x - inner_width)
              p (point.y - inner_height)
              match = false
              break
            end
          end
          if not match
            puts "Error: couldn't place panel onto an empty sheet"
            panels.delete_at idx
            next
          end
          t = translate
          used = [inner, outer]
        else
          # Otherwise, loop around the already placed panel regions and see if
          # the outline can be placed next to it.
          match = false
          placed_o.each do |face|
            # Loop through the vertices of the available region.
            face.outer_loop.vertices.each do |vertex|
              origin = vertex.position
              # Loop through each outline.
              outlines.each do |angle, inner, outer|
                # Loop through every vertex of the outline, starting from the
                # top left.
                p_idx = -1
                all_match = true
                while 1
                  p0 = outer[p_idx]
                  if not p0
                    break
                  end
                  transform = Geom::Transformation.translation ([origin.x - p0[0], origin.y - p0[1], 0])
                  if angle
                    transform = transform * Geom::Transformation.rotation(origin, Z_AXIS, angle)
                  end
                  # Check every point to see if it s within the available region.
                  all_match = true
                  inner.each do |point|
                    point = transform * point
                    px, py = point.x, point.y
                    if (px < 0) or (py < 0) or (px > inner_width) or (py > inner_height)
                      all_match = false
                      break
                    end
                    placed_o.each do |placement|
                      if placement.classify_point(point) != outside
                        all_match = false
                        break
                      end
                    end
                    if not all_match
                      break
                    end
                  end
                  # If the vertices don't overlap, check that the edges don't
                  # intersect.
                  if all_match
                    # TODO(tav): Optimise with a sweep line algorithm variant:
                    # http://en.wikipedia.org/wiki/Sweep_line_algorithm
                    outer_mapped = outer.map { |point| transform * point }
                    for i in 0...outer.length
                      p1 = outer_mapped[i]
                      p2 = outer_mapped[i+1]
                      if not p2
                        p2 = outer_mapped[0]
                      end
                      p1x, p1y = p1.x, p1.y
                      p2x, p2y = p2.x, p2.y
                      s1 = p2x - p1x
                      s2 = p2y - p1y
                      edge = [p1, [s1, s2, 0]]
                      edge_length = Math.sqrt((s1 * s1) + (s2 * s2))
                      placed_i.each do |placement|
                        placement.edges.each do |other_edge|
                          intersection = Geom.intersect_line_line edge, other_edge.line
                          if intersection
                            p3x, p3y = intersection.x, intersection.y
                            s1 = p3x - p1x
                            s2 = p3y - p1y
                            length = Math.sqrt((s1 * s1) + (s2 * s2))
                            if length > edge_length
                              next
                            end
                            s1 = p3x - p2x
                            s2 = p3y - p2y
                            length = Math.sqrt((s1 * s1) + (s2 * s2))
                            if length > edge_length
                              next
                            end
                            other_edge_length = other_edge.length
                            p4, p5 = other_edge.start.position, other_edge.end.position
                            s1 = p3x - p4.x
                            s2 = p3y - p4.y
                            length = Math.sqrt((s1 * s1) + (s2 * s2))
                            if length > other_edge_length
                              next
                            end
                            s1 = p3x - p5.x
                            s2 = p3y - p5.y
                            length = Math.sqrt((s1 * s1) + (s2 * s2))
                            if length > other_edge_length
                              next
                            end
                            all_match = false
                            break
                          end
                        end
                        if not all_match
                          break
                        end
                      end
                      if not all_match
                        break
                      end
                    end
                  end
                  if all_match
                    match = true
                    t = transform
                    used = [inner, outer]
                  end
                  p_idx -= 1
                  if match
                    break
                  end
                end
                if match
                  break
                end
              end
              if match
                break
              end
            end
            if match
              break
            end
          end
        end

        if match

          available_area -= panel_area
          inner_faces.add_face(used[0].map { |p| t * p })
          outer_faces.add_face(used[1].map { |p| t * p })
          placed_i = inner_faces.select { |e| e.typename == "Face" }
          placed_o = outer_faces.select { |e| e.typename == "Face" }

          # Generate the new loop vertices.
          loops = panel.loops.map do |loop|
            loop.map do |point|
              t * point
            end
          end
          
          

          # Generate the new circle data.
          circles = panel.circles.map do |circle|
            if circle
              center = t * circle[0]
              [center, circle[1]]
            else
              nil
            end
          end

          # Generate the new centroid.
          centroid = t * panel.centroid

          # Get the label.
          label = labels.pop

          # If this was the last label, remove the panel.
          if labels.length == 0
            panels.delete_at idx
          end

          outer_mapped = outer.map { |p| t * p }

          # Append the generated data to the current sheet.
          sheet << [loops, circles, outer_mapped, centroid, label]
          c += 1

        else

          # We do not have a match, try the next panel.
          idx += 1

        end

      end

      # If no panels could be fitted, break so as to avoid an infinite loop.
      if sheet.length == 0
        break
      end

      # Add the sheet to the collection.
      sheets << sheet

      # If there are no more panels remaining, exit the loop.
      if panels.length == 0
        break
      end

      # Wipe the generated entities.
      inner_faces.clear!
      outer_faces.clear!

    end

    # Delete the generated sheet group.
    root.erase_entities [inner_group, outer_group]

  end

end

# ------------------------------------------------------------------------------
# Panel
# ------------------------------------------------------------------------------

class WikiHousePanel

  attr_accessor :area, :centroid, :circles, :labels, :loops, :max, :min
  attr_reader :bounds_area, :error, :no_padding, :shell_area, :singleton

  def initialize(root, face, transform, labels, limits)

    # Initalise some of the object attributes.
    @error = nil
    @labels = labels
    @no_padding = false
    @singleton = false

    # Initialise a variable to hold temporarily generated entities.
    to_delete = []
    scale=WikiHouseExtension.settings["scale"]
    scale=1 if scale==0

    # Create a new face with the vertices transformed if the transformed areas
    # do not match.
    #if (face.area - face.area(transform)).abs > 0.1
      group_entity = root.add_group
      to_delete << group_entity
      group = group_entity.entities
      norm=(face.normal).normalize
      plane = [face.outer_loop.vertices[0].position,norm]
      pts=offset(face.outer_loop)
      pts=pts.map{|v| Geom::Point3d.new( v[0]*scale, v[1]*scale , v[2]*scale ).project_to_plane( plane ) }
      puts "#{labels[0]}"
      pts=check_duplicate( pts )
      tface = group.add_face(pts.map {|v| transform * v })
      face.loops.each do |loop|
        if not loop.outer?
          pts=offset(loop)
          pts=pts.map{|v| Geom::Point3d.new( v[0]*scale, v[1]*scale, v[2]*scale ).project_to_plane( plane ) }
          pts=check_duplicate( pts )
          hole = group.add_face(pts.map {|v| transform * v })
          hole.erase! if hole.valid?
        end
      end
      face = tface
    #end

    # Save the total surface area of the face.
    total_area = face.area

    # Find the normal to the face.
    normal = face.normal
    y_axis = normal.axes[1]

    # See if the face is parallel to any of the base axes.
    if normal.parallel? X_AXIS
      x, y = 1, 2
    elsif normal.parallel? Y_AXIS
      x, y = 0, 2
    elsif normal.parallel? Z_AXIS
      x, y = 0, 1
    else
      x, y = nil, nil
    end

    # Initialise the ``loops`` variable.
    loops = []

    # Initialise a reference point for transforming slanted faces.
    base = face.outer_loop.vertices[0].position

    # Loop through the edges and convert the face into a 2D polygon -- ensuring
    # that we are traversing the edges in the right order.
    face.loops.each do |loop|
      newloop = []
      if loop.outer?
        loops.insert 0, newloop
      else
        loops << newloop
      end
      edgeuse = first = loop.edgeuses[0]
      virgin = true
      prev = nil
      while 1
        edge = edgeuse.edge
        if virgin
          start = edge.start
          stop = edge.end
          next_edge = edgeuse.next.edge
          next_start = next_edge.start
          next_stop = next_edge.end
          if (start == next_start) or (start == next_stop)
            stop, start = start, stop
          elsif not ((stop == next_start) or (stop == next_stop))
            @error = "Unexpected edge connection"
            return
          end
          virgin = nil
        else
          start = edge.start
          stop = edge.end
          if stop == prev
            stop, start = start, stop
          elsif not start == prev
            @error = "Unexpected edge connection"
            return
          end
        end
        if x
          # If the face is parallel to a base axis, use the cheap conversion
          # route.
          point = start.position.to_a
          newloop << [point[x], point[y], 0]
        else
          # Otherwise, handle the case where the face is angled at a slope by
          # realigning edges relative to the origin and rotating them according
          # to their angle to the y-axis.
          point = start.position
          edge = Geom::Vector3d.new(point.x - base.x, point.y - base.y, point.z - base.z)
          if not edge.valid?
            newloop << [base.x, base.y, 0]
          else
            if edge.samedirection? y_axis
              angle = 0
            elsif edge.parallel? y_axis
              angle = Math::PI
            else
              angle = edge.angle_between y_axis
              if not edge.cross(y_axis).samedirection? normal
                angle = -angle
              end
            end
            rotate = Geom::Transformation.rotation ORIGIN, Z_AXIS, angle
            newedge = rotate * Geom::Vector3d.new(edge.length, 0, 0)
            newloop << [base.x + newedge.x, base.y + newedge.y, 0]
          end
        end
        edgeuse = edgeuse.next
        if edgeuse == first
          break
        end
        prev = stop
      end
    end

    # Initialise some more meta variables.
    areas = []
    circles = []
    cxs, cys = [], []
    intersections = []
    outer_loop = true

    # Go through the various loops calculating centroids and intersection points
    # of potential curves.
    loops.each do |loop|
      idx = 0
      intersect_points = []
      area = 0
      cx, cy = 0, 0
      while 1
        # Get the next three points on the loop.
        p1, p2, p3 = loop[idx...idx+3]
        if not p3
          if not p1
            break
          end
          if not p2
            # Loop around to the first edge.
            p2 = loop[0]
            p3 = loop[1]
          else
            # Loop around to the first point.
            p3 = loop[0]
          end
        end
        # Construct the edge vectors.
        edge1 = Geom::Vector3d.new(p2.x - p1.x, p2.y - p1.y, p2.z - p1.z)
        edge2 = Geom::Vector3d.new(p3.x - p2.x, p3.y - p2.y, p3.z - p2.z)
        intersect = nil
        if not edge1.parallel? edge2
          # Find the perpendicular vectors.
          cross = edge1.cross edge2
          vec1 = edge1.cross cross
          vec2 = edge2.cross cross
          # Find the midpoints.
          mid1 = Geom.linear_combination 0.5, p1, 0.5, p2
          mid2 = Geom.linear_combination 0.5, p2, 0.5, p3
          # Try finding an intersection.
          line1 = [mid1, vec1]
          line2 = [mid2, vec2]
          intersect = Geom.intersect_line_line line1, line2
          # If no intersection, try finding one in the other direction.
          if not intersect
            vec1.reverse!
            vec2.reverse!
            intersect = Geom.intersect_line_line line1, line2
          end
        end
        intersect_points << intersect
        if p3
          x1, y1 = p1.x, p1.y
          x2, y2 = p2.x, p2.y
          cross = (x1 * y2) - (x2 * y1)
          area += cross
          cx += (x1 + x2) * cross
          cy += (y1 + y2) * cross
        end
        idx += 1
      end
      intersections << intersect_points
      area = area * 0.5
      areas << area.abs
      cxs << (cx / (6 * area))
      cys << (cy / (6 * area))
      outer_loop = false
    end

    # Allocate variables relating to the minimal alignment.
    bounds_area = nil
    bounds_min = nil
    bounds_max = nil
    transform = nil
    outer = loops[0]

    # Unpack panel dimension limits.
    panel_height, panel_width, panel_max_height, panel_max_width, padding = limits

    # Try rotating at half degree intervals and find the transformation which
    # occupies the most minimal bounding rectangle.
    (0...180.0).step(0.5) do |angle|
      t = Geom::Transformation.rotation ORIGIN, Z_AXIS, angle.degrees
      bounds = Geom::BoundingBox.new
      outer.each do |point|
        point = t * point
        bounds.add point
      end
      min, max = bounds.min, bounds.max
      height = max.y - min.y
      width = max.x - min.x
      if (height - panel_height) > 0.1
        next
      end
      if (width - panel_width) > 0.1
        next
      end
      area = width * height
      if (not bounds_area) or ((bounds_area - area) > 0.1)
        bounds_area = area
        bounds_min, bounds_max = min, max
        transform = t
      end
    end
    
    # If we couldn't find a fitting angle, try again at 0.1 degree intervals.
    if not transform
      (0...180.0).step(0.1) do |angle|
        t = Geom::Transformation.rotation ORIGIN, Z_AXIS, angle.degrees
        bounds = Geom::BoundingBox.new
        outer.each do |point|
          point = t * point
          bounds.add point
        end
        min, max = bounds.min, bounds.max
        height = max.y - min.y
        width = max.x - min.x
        if (width - panel_max_width) > 0.1
          #HBL nextm
          next
        end
        if (height - panel_max_height) > 0.1
          next
        end
        area = width * height
        if (not bounds_area) or ((bounds_area - area) > 0.1)
          bounds_area = area
          bounds_min, bounds_max = min, max
          transform = t
        end
      end
    end

    # If we still couldn't find a fitting, abort.
    if not transform
      @error = "Couldn't fit panel within cutting sheet"
      puts @error
      return
    end

    # Set the panel to a singleton panel (i.e. without any padding) if it is
    # larger than the height and width, otherwise set the no_padding flag.
    width = bounds_max.x - bounds_min.x
    height = bounds_max.y - bounds_min.y
    if (width + padding) > panel_width
      @no_padding = 'w'
    end
    if (height + padding) > panel_height
      if @no_padding
        @singleton = true
        @no_padding = nil
      else
        @no_padding = 'h'
      end
    end

    # Transform all points on every loop.
    loops.map! do |loop|
      loop.map! do |point|
        transform * point
      end
    end

    # Find the centroid.
    @shell_area = surface_area = areas.shift
    topx = surface_area * cxs.shift
    topy = surface_area * cys.shift
    for i in 0...areas.length
      area = areas[i]
      topx -= area * cxs[i]
      topy -= area * cys[i]
      surface_area -= area
    end
    cx = topx / surface_area
    cy = topy / surface_area
    centroid = transform * [cx, cy, 0]

    # Sanity check the surface area calculation.
    if (total_area - surface_area).abs > 0.1
      @error = "Surface area calculation differs"
      return
    end

    # TODO(tav): We could also detect arcs once we figure out how to create
    # polylined shapes with arcs in the DXF output. This may not be ideal as
    # polyarcs may also cause issues with certain CNC routers.

    # Detect all circular loops.
    for i in 0...loops.length
      points = intersections[i]
      length = points.length
      last = length - 1
      circle = true
      for j in 0...length
        c1 = points[j]
        c2 = points[j+1]
        if j == last
          c2 = points[0]
        end
        if not (c1 and c2)
          circle = false
          break
        end
        if ((c2.x - c1.x).abs > 0.1) or ((c2.y - c1.y).abs > 0.1)
          circle = false
          break
        end
      end
      if circle and length >= 24
        center = transform * points[0]
        p1 = loops[i][0]
        x = center.x - p1.x
        y = center.y - p1.y
        radius = Math.sqrt((x * x) + (y * y))
        circles[i] = [center, radius]
      end
    end
    
    #Place outer at the end
    outerloop=loops.shift
    #puts "outerloop: #{outerloop}"
    loops.push(outerloop)
    
    # Save the generated data.
    @area = total_area
    @bounds_area = bounds_area
    @centroid = centroid
    @circles = circles
    @loops = loops
    @max = bounds_max
    @min = bounds_min

    # Delete any temporarily generated groups.
    if to_delete.length > 0
      root.erase_entities to_delete
    end

  end
  
  # ------------------------------------------------------------------------------
  # offset
  # ------------------------------------------------------------------------------
  # Inspired from offset.rb
  # Copyright 2004,2005,2006,2009 by Rick Wilson - All Rights Reserved
  # ------------------------------------------------------------------------------
  def offset( loop )
    verts=loop.vertices;pts = [];vecs = []
    #return verts.map {|v| v.position } if WikiHouseExtension.settings["drill_width"]== 0
     if loop.outer?
         dist=(WikiHouseExtension.settings["drill_width"]/2)
     else
         dist=-(WikiHouseExtension.settings["drill_width"]/2)
     end
     #puts "drill_width: #{WikiHouseExtension.settings["drill_width"]}"
	 # CREATE ARRAY pts OF OFFSET POINTS FROM FACE
	 #puts "verts.length: #{verts.length}\n"
	 0.upto(verts.length-1) do |a|
			 vec1 = (verts[a].position-verts[a-(verts.length-1)].position).normalize
			 vec2 = (verts[a].position-verts[a-1].position).normalize
			 next if vec2.parallel?(vec1) 
			 vec3 = (vec1+vec2).normalize
			 #puts "vec3: #{vec3}\n"al
			 if vec3.valid?
				 ang = vec1.angle_between(vec2)/2
				 ang = Math::PI/2 if vec1.parallel?(vec2)
				 vec3.length = dist/Math::sin(ang)
				 vecs << vec3
				 t = Geom::Transformation.new(vec3)
				 if pts.length > 0
					 vec4 = pts.last.vector_to(verts[a].position.transform(t))
					 if vec4.valid?
							 unless (vec2.parallel?(vec4))
									t = Geom::Transformation.new(vec3.reverse)
									#puts "vec3b: #{vec3} "
							 end
					 end
				 end
						#puts "vec3: #{vec3} "
						vert=verts[a].position.transform(t)
						pts.push(vert.to_a)
				 else
						puts "#{a} - vec3 is invalid"
				 end
			 end
  (pts.length > 2) ? (return pts) : (return nil)
  end
  
  def check_duplicate( pts )
  			 # CHECK FOR DUPLICATE POINTS IN pts ARRAY
			 duplicates = []
			 loop=0
			 pts.each_index do |a|
				 pts.each_index do |b|
						next if b==a
						if pts[a]===pts[b] and loop > 0
						  duplicates << b
						else
						 loop=loop+1
						end
				 end
					break if a==pts.length-1
			 end
			 duplicates.reverse.each{|a| pts.delete(pts[a])}
				# CREATE CURVE FROM POINTS IN pts ARRAY
				#puts "pts: #{pts}\n"
				(pts.length > 2) ? (pts.push pts[0] if loop = 0;return pts) : (return nil)
 end
end #Class

# ------------------------------------------------------------------------------
# Entities Loader
# ------------------------------------------------------------------------------
  
class WikiHouseEntities

  attr_accessor :orphans, :panels

  def initialize(entities, root, dimensions)

    $count_s1 = 0
    $count_s2 = 0
    $count_s3 = 0
    $count_s4 = 0

    # Initialise the default attribute values.
    @faces = Hash.new
    @groups = groups = Hash.new
    @orphans = orphans = Hash.new
    @root = root
    @to_delete = []
    @todo = todo = []

    # Set a loop counter variable and the default identity transformation.
    loop = 0
    transform = Geom::Transformation.new

    # Aggregate all the entities into the ``todo`` array.
    entities.each { |entity| todo << [entity, transform] }

    # Visit all component and group entities defined within the model and count
    # up all orphaned face entities.
    while todo.length != 0
      Sketchup.set_status_text WIKIHOUSE_DETECTION_STATUS[(loop/10) % 5]
      loop += 1
      entity, transform = todo.pop
      case entity.typename
      when "Group", "ComponentInstance"
        visit entity, transform
      when "Face"
        if orphans[WIKIHOUSE_DUMMY_GROUP]
          orphans[WIKIHOUSE_DUMMY_GROUP] += 1
        else
          orphans[WIKIHOUSE_DUMMY_GROUP] = 1
        end
      end
    end

    # If there were no orphans, unset the ``@orphans`` attribute.
    if not orphans.length > 0
      @orphans = nil
    end

    # Reset the loop counter.
    loop = 0

    # Construct the panel limit dimensions.
    height, width, padding = [dimensions[2], dimensions[3], dimensions[5]]
    padding = 2 * padding
    limits = [height - padding, width - padding, height, width, padding]

    # Loop through each group and aggregate parsed data for the faces.
    @panels = items = []
    @faces.each_pair do |group, faces|
      meta = groups[group]
      sample = faces[0]
      if meta.length == 1
        f_data = { meta[0][0] => [meta[0][1]] }
      else
        f_data = Hash.new
        meta = meta.map { |t, l| [t, l, sample.area(t)] }.sort_by { |t| t[2] }
        while meta.length != 0
          t1, l1, a1 = meta.pop
          idx = -1
          f_data[t1] = [l1]
          while 1
            f2_data = meta[idx]
            if not f2_data
              break
            end
            t2, l2, a2 = f2_data
            if (a2 - a1).abs > 0.1
              break
            end
            f_data[t1] << l2
            meta.delete_at idx
          end
        end
      end
      f_data.each_pair do |transform, labels|
        panels = faces.map do |face|
          Sketchup.set_status_text WIKIHOUSE_PANEL_STATUS[(loop/3) % 5]
          loop += 1
          WikiHousePanel.new root, face, transform, labels, limits
        end
        items.concat panels
      end
    end

    total = 0
    items.each { |item| total += item.labels.length }

    if @orphans
      puts "Orphans: #{@orphans.length} Groups"
    end

    puts "Items: #{total}"
    puts "S1: #{$count_s1}"
    puts "S2: #{$count_s2}"
    puts "S3: #{$count_s3}"
    puts "S4: #{$count_s4}"

  end

  
def visit_faces(faces, transform)

    # Handle the case where no faces have been found or just a single orphaned
    # face exists.
    if faces.length <= 1
      if faces.length == 0
        return [], nil
      else
        return [], faces
      end
    end

    # Define some local variables.
    found = []
    orphans = []

    # Sort the faces by their respective surface areas in order to minimise
    # lookups.
    faces = faces.sort_by { |face| face.area transform }

    # Iterate through the faces and see if we can find matching pairs.
    while faces.length != 0
      face1 = faces.pop
      area1 = face1.area transform
      # Ignore small faces.
      if area1 < 5
        next
      end
      idx = -1
      match = false
      # Check against all remaining faces.
      while 1
        face2 = faces[idx]
        if not face2
          break
        end
        if face1 == face2
          faces.delete_at idx
          next
        end
        # Check that the area of both faces are close enough -- accounting for
        # any discrepancies caused by floating point rounding errors.
        area2 = face2.area transform
        diff = (area2 - area1).abs
        if diff < 0.5 # TODO(tav): Ideally, this tolerance will be 0.1 or less.
          $count_s1 += 1
          # Ensure that the faces don't intersect, i.e. are parallel to each
          # other.
          intersect = Geom.intersect_plane_plane face1.plane, face2.plane
          if intersect
            # Calculate the angle between the two planes and accomodate for
            # rounding errors.
            angle = face1.normal.angle_between face2.normal
            if angle < 0.01
              intersect = nil
            elsif (Math::PI - angle).abs < 0.01
              intersect = nil
            end
          end
          if not intersect
            $count_s2 += 1
            vertices1 = face1.vertices
            vertices2 = face2.vertices
            vertices_length = vertices1.length
            # Check if both faces have matching number of outer vertices and
            # that they each share a common edge.
            vertices1 = face1.outer_loop.vertices
            vertices2 = face2.outer_loop.vertices
            for i in 0...vertices1.length
              vertex1 = vertices1[i]
              connected = false
              for j in 0...vertices2.length
                vertex2 = vertices2[j]
                if vertex1.common_edge vertex2
                  connected = true
                  vertices2.delete_at j
                  break
                end
              end
              if not connected
                break
              end
            end
            if connected
              $count_s3 += 1
              # Go through the various loops of edges and find ones that have
              # shared edges to the other face.
              loops1 = []
              loops2 = []
              loops2_lengths = []
              face2.loops.each do |loop|
                if not loop.outer?
                  loops2 << loop
                  loops2_lengths << loop.vertices.length
                end
              end
              face1_loops = face1.loops
              face1_loops.each do |loop1|
                if not loop1.outer?
                  loop1_vertices = loop1.vertices
                  loop1_length = loop1_vertices.length
                  for l in 0...loops2.length
                    if loops2_lengths[l] == loop1_length
                      loop2_vertices = loops2[l].vertices
                      for i in 0...loop1_length
                        v1 = loop1_vertices[i]
                        connected = false
                        for j in 0...loop2_vertices.length
                          v2 = loop2_vertices[j]
                          if v1.common_edge v2
                            connected = true
                            loop2_vertices.delete_at j
                            break
                          end
                        end
                        if not connected
                          break
                        end
                      end
                      if connected
                        loops1 << loops2[l].vertices
                        loops2.delete_at l
                        loops2_lengths.delete_at l
                        break
                      end
                    end
                  end
                end
              end
              # If the number of loops with shared edges don't match up with the
              # original state, create a new face.
              if loops1.length != (face1.loops.length - 1)
                group = @root.add_group
                group_ents = group.entities
                face = group_ents.add_face vertices1
                loops1.each do |v|
                  hole = group_ents.add_face v
                  hole.erase! if hole.valid?
                end
                @to_delete << group
              else
                face = face1
              end
              # We have matching and connected faces!
              match = true
              found << face
              faces.delete_at idx
              if WIKIHOUSE_HIDE
                face1.hidden = true
                face2.hidden = true
              end
              break
            end
          end
        end
        idx -= 1
      end
      if match
        next
      end
      orphans << face1
    end

    # Return all the found and orphaned faces.
    return found, orphans

  end
  
def visit(group, transform)

    # Setup some local variables.
    exists = false
    faces = []
    groups = @groups

    # Setup the min/max heights for the depth edge/faces.
    min_height =  WikiHouseExtension.settings["sheet_depth"]-1.mm
    max_height =  WikiHouseExtension.settings["sheet_depth"]+1.mm

    # Apply the transformation if one has been set for this group.
    if group.transformation
      transform = transform * group.transformation
    end

    # Get the label.
    label = group.name
    if label == ""
      label = nil
    end

    # Get the entities set.
    case group.typename
    when "Group"
      entities = group.entities
    else
      group = group.definition
      entities = group.entities
      # Check if we've seen this component before, and if so, reuse previous
      # data.
      if groups[group]
        groups[group] << [transform, label]
        entities.each do |entity|
          case entity.typename
          when "Group", "ComponentInstance"
            @todo << [entity, transform]
          end
        end
        return
      end
    end

    # Add the new group/component definition.
    groups[group] = [[transform, label]]

    # Loop through the entities.
    entities.each do |entity|
      case entity.typename
      when "Face"
        edges = entity.edges
        ignore = 0
        # Ignore all faces which match the specification for the depth side.
        if edges.length == 4
          for i in 0...4
            edge = edges[i]
            length = edge.length
            if length < max_height and length > min_height
              ignore += 1
              if ignore == 2
                break
              end
            end
          end
        end
        if WIKIHOUSE_HIDE and ignore == 2
          entity.hidden = false
        end
        if ignore != 2 # TODO(tav): and entity.visible?
          faces << entity
        end
      when "Group", "ComponentInstance"
        # Append the entity to the todo attribute instead of recursively calling
        # ``visit`` so as to avoid blowing the stack.
        @todo << [entity, transform]
      end
    end

    faces, orphans = visit_faces faces, transform

    if orphans and orphans.length > 0
      @orphans[group] = orphans.length
    end

    if faces and faces.length > 0
      @faces[group] = faces
    end

  end
  

  def purge

    # Delete any custom generated entity groups.
    if @to_delete and @to_delete.length != 0
      @root.erase_entities @to_delete
    end

    # Nullify all container attributes.
    @faces = nil
    @groups = nil
    @orphans = nil
    @root = nil
    @to_delete = nil
    @todo = nil

  end

end


# ------------------------------------------------------------------------------
# Make This House
# ------------------------------------------------------------------------------

def self.make_wikihouse(model, interactive)

  # Isolate the entities to export.
  entities = root = model.active_entities
  selection = model.selection
  if selection.empty?
    if interactive
      reply = UI.messagebox "No objects selected. Export the entire model?", MB_OKCANCEL
      if reply != REPLY_OK
        return
      end
    end
  else
    entities = selection
  end

  dimensions = WikiHouseExtension.dimensions_inch()

  # Load and parse the entities.
  if WIKIHOUSE_SHORT_CIRCUIT and $wikloader
    loader = $wikloader
  else
    loader = WikiHouseEntities.new entities, root, dimensions
    if WIKIHOUSE_SHORT_CIRCUIT
      $wikloader = loader
    end
  end

  if interactive and loader.orphans
    msg = "The cutting sheets may be incomplete. The following number of faces could not be matched appropriately:\n\n"
    loader.orphans.each_pair do |group, count|
      msg += "    #{count} in #{group.name.length > 0 and group.name or 'Group#???'}\n"
    end
    UI.messagebox msg
    puts msg
  end

  # Filter out any panels which raised an error.
  panels = loader.panels.select { |panel| !panel.error }

  # Run the detected panels through the layout engine.
  layout = WikiHouseLayoutEngine.new panels, root, dimensions

  # Generate the SVG file.
  svg = WikiHouseSVG.new layout, WIKIHOUSE_SCALE
  svg_data = svg.generate

  # Generate the DXF file.
  dxf = WikiHouseDXF.new layout
  dxf_data = dxf.generate

  # Cleanup.
  Sketchup.set_status_text ""
  loader.purge

  # Return the generated data.
  [svg_data, dxf_data]

end




# ------------------------------------------------------------------------------
# Make Dialog
# ------------------------------------------------------------------------------
def self.load_wikihouse_make

  model = Sketchup.active_model

  # Exit if a model wasn't available.
  if not model
    show_wikihouse_error "You need to open a SketchUp model before it can be fabricated"
    return
  end

  # Initialise an attribute dictionary for custom metadata.
  attr = model.attribute_dictionary WIKIHOUSE_TITLE, true
  if attr.size == 0
    attr["spec"] = WIKIHOUSE_SPEC
  end

  # Exit if it's an unsaved model.
  model_path = model.path
  if model_path == ""
    UI.messagebox "You need to save the model before the cutting sheets can be generated"
    return
  end

  # Try and infer the model's filename.
  filename = model.title
  if filename == ""
    filename = "Untitled"
  end

  # Get the model's parent directory and generate the new filenames to save to.
  directory = File.dirname(model_path)
  svg_filename = File.join(directory, filename + ".svg")
  #dxf_filename = File.join(directory, filename + ".dxf")

  # Make the cutting sheets for the house!
  data = make_wikihouse model, true
  if not data
    return
  end

  svg_data, dxf_data = data

  # Save the SVG data to the file.
  File.open(svg_filename, "wb") do |io|
    io.write svg_data
  end

  # Save the DXF data to the file.
  #File.open(dxf_filename, "wb") do |io|
    #io.write dxf_data
  #end

  UI.messagebox "Cutting sheets successfully saved to #{directory}", MB_OK

  dialog = UI::WebDialog.new "Cutting Sheets Preview", true, "#{WIKIHOUSE_TITLE}-Preview", 800, 800, 150, 150, true
  dialog.set_file svg_filename
  if WIKIHOUSE_MAC
    dialog.show_modal
  else
    dialog.show
  end
end


# ------------------------------------------------------------------------------
# set_colors
# ------------------------------------------------------------------------------
def self.colors(model)
	back_material=nil
	front_material=nil
    materials = model.materials
    color_back = Sketchup::Color.new(105,105,105)
    color_front = Sketchup::Color.new(245,245,245)
    back_material = materials.find{ |material| material.name == 'back'}
    back_material = materials.add('back') if back_material.nil? or  back_material.name != 'back'
    front_material = materials.find { |material| material.name == 'front'}
    front_material = materials.add('front') if front_material.nil? or front_material.name != 'front'
    back_material.color = color_back
    front_material.color = color_front
    WikiHouseExtension.back_material=back_material
    WikiHouseExtension.front_material=front_material
end


# ------------------------------------------------------------------------------
# visit_entities
# ------------------------------------------------------------------------------
def self.visit_entities(model,group, transform , groups, todo, faces , pathes , unique )
    # Setup some local variables.
    exists = false
    fs = []
    vfaces = []

    # Setup the min/max heights for the depth edge/faces.
    min_height = WikiHouseExtension.settings["sheet_depth"]-1.mm
    max_height = WikiHouseExtension.settings["sheet_depth"]+1.mm

    # Get the label.
    label="None"
    
    if group.transformation
           transform = transform * group.transformation
    end
     
    # Get the entities set.
    case group.typename
     when "Group"
      entities = group.entities
      label = group.name if not group.name.nil?
     else #Component
      group=group.make_unique if unique
      entities = group.definition.entities
      label = group.name if not group.name.nil?
    end

    # Add the new group/component definition.
    pathes[group]=label if pathes[group].nil?
    groups[group] = [[transform, label]]

    # Loop through the entities.
    entities.each do |entity|
      case entity.typename
      when "Face"
      	# List vertical faces
        edges = entity.edges
        ignore = 0
        if edges.length == 4
          for i in 0...4
            edge = edges[i]
            length = edge.length
            if length < max_height and length > min_height
              ignore += 1
              if ignore == 2
                break
              end
            end
          end
        end
        if ignore != 2
          entity.back_material=WikiHouseExtension.back_material if not WikiHouseExtension.back_material.deleted?
          fs << entity
          #puts "fs: #{fs}\n"
        end
      when "Group"
        # Append the entity to the todo attribute instead of recursively calling
        # ``visit`` so as to avoid blowing the stack.
        todo << [entity, transform]
        pathes[entity]=pathes[group]+"\\"+entity.name
      when "ComponentInstance"
        # Append the entity to the todo attribute instead of recursively calling
        # ``visit`` so as to avoid blowing the stack.
        todo << [entity, transform]
        pathes[entity]=pathes[group]+"\\"+entity.name
      end
    end
  
  if not fs.nil? and fs.length > 0
      faces[group] = fs
  end
end



# ------------------------------------------------------------------------------
# CLASS: WikiHouseUnion
# ------------------------------------------------------------------------------
# Author: BAILLY H. - www.DoNovae.com
# Date : 30/09/2015
# ------------------------------------------------------------------------------
class WikiHouseUnion
 def initialize(model , entities , filename)
     # Initialise the default attribute values.
    @faces = Hash.new
    @groups = groups = Hash.new
    @pathes = Hash.new
    @orphans = orphans = Hash.new
    @root = model
    @to_delete = []
    @todo = todo = []
    @name=nil
    @material=nil

    WikiHouseExtension.colors(model)
    min_height =  WikiHouseExtension.settings["sheet_depth"]-1.mm
    max_height =  WikiHouseExtension.settings["sheet_depth"]+1.mm
    puts "min_height: #{min_height.to_mm}"
    puts "max_height: #{max_height.to_mm}"

    # Set a loop counter variable and the default identity transformation.
    loop = 0
    transform = Geom::Transformation.new
    
    # Aggregate all the entities into the ``todo`` array.
    entities.each { |entity| todo << [entity, transform] }
    
    # Visit all component and group entities defined within the model and count
    # up all orphaned face entities.
    while todo.length != 0
      Sketchup.set_status_text WIKIHOUSE_DETECTION_STATUS[(loop/10) % 5]
      loop += 1
      entity, transform = todo.pop
      case entity.typename
      when "Group", "ComponentInstance"
      	@material=entity.material if @material.nil?
        @name=entity.name if @name.nil?
        #puts "---> material = #{@material.name}"
        WikiHouseExtension.visit_entities(model, entity, transform, @groups, @todo, @faces , @pathes , false )
      when "Face"
        if orphans[WIKIHOUSE_DUMMY_GROUP]
          orphans[WIKIHOUSE_DUMMY_GROUP] += 1
        else
          orphans[WIKIHOUSE_DUMMY_GROUP] = 1
        end
      end
    end
    
    # union
    @material=model.materials.at(0) if  @material.nil?
    union model
 end
# ------------------------------------------------------------------------------
# union
# ------------------------------------------------------------------------------
def union( model )
    layers = model.layers
    ly0 = layers.add LAYER0
    ly0.visible = true
    ly_inner = layers.add LAYER_INNER
    ly_inner.visible = true
    ly_outer = layers.add LAYER_OUTER
    ly_outer.visible = true
    gp = Sketchup.active_model.entities.add_group
    gp_entities = gp.entities
    ofaces=[]
    ifaces=[]
    transform=nil
    dir=nil
    face_ref=nil
    identity=Geom::Transformation.new
    model.active_layer = ly0
    filename = model.title
    if filename == ""
     filename = "Untitled"
    end
    
    #Normal ref
    keys=@faces.keys
    group0=keys[0]
    faces0=@faces.fetch(group0)
    faces_ref=faces0.sort_by { |face| face.area }.reverse
    faces_ref_a=faces_ref.to_a
    #puts "faces_ref_a.aera: #{(faces_ref_a.map{|face| face.area})}"
    #puts "faces_ref_a.normal: #{(faces_ref_a.map{|face| face.normal})}"
    face_ref=faces_ref_a.find { |f| f!=nil }
    gp.name=@name+'#1'
    transform0=group0.transformation
    ref_normal=(transform0*face_ref.normal).normalize
    puts "ref_normal: #{ref_normal}"
    loop0=faces0[0].outer_loop
    loop0.edges.map{|edge| edge.visible=true}
    plane = [transform0*loop0.vertices[0].position,ref_normal]
    
    @faces.each_pair do |group,faces|
      transform=group.transformation
       #loop0=faces0[0].outer_loop
       #loop0.edges.map{|edge| edge.visible=true}
       #plane = [transform0*loop0.vertices[0].position,ref_normal]
       faces.each do |face|
          ofaces0=[]
          ifaces0=[]
          # Select only uper faces
          norm=(transform*face.normal).normalize
          if (ref_normal.dot norm ) > 0.5
            #loop=face.outer_loop
            #loop.edges.map{|edge| edge.visible=true}
            #plane = [transform*loop.vertices[0].position,norm]
            face.loops.each do |loop|  
              if not loop.outer?
                pts=loop.vertices.map {|v| transform * v.position }
                pts=pts.map{|v| [v.x=(v.x).round(4),v.y=(v.y).round(4),v.z=(v.z).round(4)]}
                pts.uniq!
                if pts.length > 2
                  pts<<pts[0]
                  pts=pts.map{|v| Geom::Point3d.new( v[0], v[1] , v[2] ).project_to_plane( plane ) }
                  #puts "pts: #{pts}"
                  newf=gp_entities.add_face(pts)
                  newf.layer=ly0
                  ifaces0 << newf
                end
              end
            end #loop
             loop=face.outer_loop
             pts=loop.vertices.map {|v| transform * v.position }
             pts=pts.map{|v| [v.x=(v.x).round(4),v.y=(v.y).round(4),v.z=(v.z).round(4)]}
             pts.uniq!
             if pts.length > 2
               pts<<pts[0]
               #puts "pts: #{pts}"
               pts=pts.map{|v| Geom::Point3d.new( v[0], v[1] , v[2] ) }
               pts=pts.map{|v| Geom::Point3d.new( v[0], v[1] , v[2] ).project_to_plane( plane ) }
               newf=gp_entities.add_face(pts)
               newf.layer=ly0
               newf.material=WikiHouseExtension.front_material
               newf.material=@material if not @material.nil?
               newf.back_material=WikiHouseExtension.back_material
               ofaces0 << newf
             end
          end #if
          ifaces0.each { |face| face.erase! if not face.deleted?}
          ofaces.concat(ofaces0)
          next
       end #faces.each
      end #faces.each_pair
     
      # Extrusion
      ofaces.each do |face|
          if not face.deleted?
               norm=(face.normal).normalize
               face.reverse! if  ref_normal.dot(norm )< 0 
      	       face.pushpull( WikiHouseExtension.settings["sheet_depth"] , false )
      	  end
      end
      
      # Suppressions
      suppress_dummy_edges( ofaces )
      suppress_isolated_edges( gp_entities )
      model.active_layer = ly0
      
      model.selection.clear
      model.selection.add gp
      layers = model.layers
      status = layers.purge_unused
      puts "Union: performed"
end

# ------------------------------------------------------------------------------
# suppress_isolated_edges
# ------------------------------------------------------------------------------
def suppress_isolated_edges( entities )
      edges=[]
      entities.each do |ent|
         if ent.typename=='Edge' and ent.faces.length == 0
             edges << ent
         end
      end
      edges.each { |ent| ent.erase! if not ent.deleted?}
end

# ------------------------------------------------------------------------------
# suppress_dummy_edges
# ------------------------------------------------------------------------------
def suppress_dummy_edges( ofaces )
      edgetodel=[]
       facetodel=[]
      ofaces.each do |face|
      	if (not face.deleted?)
      	  if ( face.normal.angle_between Z_AXIS ).abs < 0.1 or ((face.normal.angle_between Z_AXIS ) - Math::PI ).abs < 0.1
      	    face.loops.each do |loop|       
              if loop.outer?
              	      loop.edges.each do |edge|
              	      	      nbh=0
              	      	      nbv=0
              	      	      facev=nil
              	      	      edge.faces.each do |f|
              	      	      	if ( f.normal.angle_between Z_AXIS ).abs < 0.1 or ((f.normal.angle_between Z_AXIS ) - Math::PI ).abs < 0.1
              	      	      	      nbh=nbh+1 
              	      	      	 end
              	      	      	 if (( f.normal.angle_between Z_AXIS ) - Math::PI / 2.0 ).abs < 0.1 or (( f.normal.angle_between Z_AXIS ) - 3.0 * Math::PI / 2.0 ).abs < 0.1
              	      	      	 	 nbv=nbv+1
              	      	      	 	 facev=f
              	      	      	 end
              	      	      end
              	      	      if nbh == 2 and nbv == 1
              	      	      	     #puts "---> nbh = #{nbh} - nbv = #{nbv}"
              	      	      	     facev.edges.each { |edge| edgetodel << edge }
              	      	      	     edgetodel << edge
              	      	      end
              	      end
              end
          end
        end
      end
      end
      edgetodel.each { |ent| ent.erase! if not ent.deleted?}
end
end #CLASS



# ------------------------------------------------------------------------------
# wikihouse_union
# ------------------------------------------------------------------------------
def self.wikihouse_union ( model , filename , interactive )
  entities = model.active_entities
  selection = model.selection
  if selection.empty?
    if interactive
      reply = UI.messagebox "No objects selected. Export the entire model?", MB_OKCANCEL
      if reply != REPLY_OK
        return
      end
    end
  else
    entities = selection
  end
    WikiHouseUnion.new model , entities , filename
end


# ------------------------------------------------------------------------------
# load_wikihouse_union
# ------------------------------------------------------------------------------
def self.load_wikihouse_union 
  model = Sketchup.active_model

  # Exit if a model wasn't available.
  if not model
    show_wikihouse_error "You need to open a SketchUp model before it can be fabricated"
    return
  end

  # Initialise an attribute dictionary for custom metadata.
  attr = model.attribute_dictionary WIKIHOUSE_TITLE, true
  if attr.size == 0
    attr["spec"] = WIKIHOUSE_SPEC
  end

  # Exit if it's an unsaved model.
  model_path = model.path
  if model_path == ""
    UI.messagebox "You need to save the model before the cutting sheets can be generated"
    return
  end

  # Try and infer the model's filename.
  filename = model.title
  if filename == ""
    filename = "Untitled"
  end

  # Get the model's parent directory and generate the new filenames to save to.
  directory = File.dirname(model_path)

  # Union
  data = wikihouse_union model, filename ,true
  if not data
    return
  end

end

# ------------------------------------------------------------------------------
# CLASS: WikiHouseAlign
# ------------------------------------------------------------------------------
# Author: BAILLY H. - www.DoNovae.com
# Date : 30/09/2015
# ------------------------------------------------------------------------------

class WikiHouseAlign
# ------------------------------------------------------------------------------
# initialize
# ------------------------------------------------------------------------------
def initialize(model , entities )
     # Initialise the default attribute values.
    @POSITION=0
    @NORMAL=1
    @DIRECTION=2
    @faces = Hash.new
    @pathes = Hash.new
    @groups = groups = Hash.new
    @orphans = orphans = Hash.new
    @root = model
    @to_delete = []
    @todo = todo = []
    @positions = Hash.new
    @ref_position = Hash.new
    @faces1234 = Hash.new
    @vfaces = Hash.new

    WikiHouseExtension.colors(model)
    min_height =  WikiHouseExtension.settings["sheet_depth"]-1.mm
    max_height =  WikiHouseExtension.settings["sheet_depth"]+1.mm
    puts "min_height: #{min_height.to_mm}"
    puts "max_height: #{max_height.to_mm}"

    # Set a loop counter variable and the default identity transformation.
    loop = 0
    transform = Geom::Transformation.new
    
    # Aggregate all the entities into the ``todo`` array.
    entities.each { |entity| todo << [entity, transform] }
    
    # Visit all component and group entities defined within the model and count
    # up all orphaned face entities.
    while todo.length != 0
      Sketchup.set_status_text WIKIHOUSE_DETECTION_STATUS[(loop/10) % 5]
      loop += 1
      entity, transform = todo.pop
      case entity.typename
      when "Group", "ComponentInstance"
        @material=entity.material if @material.nil?
        @name=entity.name if @name.nil?
        WikiHouseExtension.visit_entities(model, entity, transform, @groups, @todo, @faces , @pathes, false )
         #puts "---> material = #{material.name}"
      when "Face"
        if orphans[WIKIHOUSE_DUMMY_GROUP]
          orphans[WIKIHOUSE_DUMMY_GROUP] += 1
        else
          orphans[WIKIHOUSE_DUMMY_GROUP] = 1
        end
      end
    end
    
    # Align
    align model
end




# ------------------------------------------------------------------------------
# position
# ------------------------------------------------------------------------------
def position ( face , transform )
    pos=position2_a( face , transform )
    return Geom::Point3d.new(pos)
end

def position2_a ( face , transform )
   position=[0,0,0]
   #puts "face: #{face}\n"
   face.loops.each do |loop|
              if loop.outer?
                pts=loop.vertices.map {|v| transform * v.position }
                pts.each do |pt|
                	position=position.zip(pt.to_a).map { |z| z.inject(&:+) }
                end
                position=position.collect { |x| x / loop.vertices.length }
                #puts "position: #{position}"
                return position
              end
    end #loop
    return position
end

# ------------------------------------------------------------------------------
# div2_a
# ------------------------------------------------------------------------------
def div2_a ( pt1 , value )
    pt1_a=pt1.to_a
    return pt1_a.collect { |x| x / value }
end


# ------------------------------------------------------------------------------
# mul2_a
# ------------------------------------------------------------------------------
def mul2_a ( pt1 , value )
    pt1_a=pt1.to_a
    return pt1_a.collect { |x| x * value }
end


# ------------------------------------------------------------------------------
# add2_a
# ------------------------------------------------------------------------------
def add2_a ( pt1 , pt2 )
    pt1_a=pt1.to_a
    pt2_a=pt2.to_a
    return pt1_a.zip(pt2_a).map { |z| z.inject(&:+) }
end

# ------------------------------------------------------------------------------
# sub2_a
# ------------------------------------------------------------------------------
def sub2_a ( pt1 , pt2 )
    pt1_a=pt1.to_a
    pt2_a=pt2.to_a
    return pt1_a.zip(pt2_a).map { |z| z.inject(&:-) }
end


# ------------------------------------------------------------------------------
# Draw cube
# ------------------------------------------------------------------------------
def cube (  name , lbb , rbb , lfb , lbt )
  model=Sketchup.active_model
  group = model.entities.add_group
  group.name=name
  group.entities.add_face lbb , lfb , lbt
  group.entities.add_face lbb , rbb , lbt
end


# ------------------------------------------------------------------------------
# direction
# ------------------------------------------------------------------------------
def direction2_a ( ifgp0 , group0 , group )
    direction=Hash.new
    face0=@faces.fetch(group0)
    face=@faces.fetch(group)
    if ifgp0
        faces1234=@faces1234[group0]
    else
        faces1234=@faces1234[group]
    end
    #puts "group: #{group.name}\n"
    #puts "faces1234: #{faces1234}\n"
    transform0=group0.transformation
    transform=group.transformation
    pos0=Geom::Vector3d.new position2_a( face0 , transform0 )
    pos=Geom::Vector3d.new position2_a( face , transform )
    dir=Geom::Vector3d.linear_combination(1, pos, -1, pos0)
    #puts "pos0: #{pos0}\n"
    #puts "pos: #{pos}\n"
    #puts "dir: #{dir}\n"
    dots=faces1234.keys.map { |v| dir.dot v.normalize  }
    #puts "dots: #{dots}\n\n"
    idmax=dots.index(dots.max)
    idmin=dots.index(dots.min)
    #puts "idmax: #{idmax}\n"
    #puts "idmin: #{idmin}\n"
    vecpos=Hash.new
    vecpos["Vec"]=faces1234.keys[ idmax ]
    vecpos["Pos"]=faces1234[faces1234.keys[ idmax ]]
    direction["Front"]=vecpos
    vecpos=Hash.new
    vecpos["Vec"]=faces1234.keys[ idmin ]
    vecpos["Pos"]=faces1234[faces1234.keys[ idmin ]]
    direction["Rear"]=vecpos
    #puts "direction: #{direction}"
    return direction
end


# ------------------------------------------------------------------------------
# Boxes
# ------------------------------------------------------------------------------
def boxe( id , group )
      box = group.bounds
      lbb=box.corner(2)
      rbb=box.corner(3)
      lfb=box.corner(0)
      lbt=box.corner(6)
      name="Groupe"+(id).to_s
      cube( name, lbb , rbb, lfb , lbt)
end



# ------------------------------------------------------------------------------
# move
# ------------------------------------------------------------------------------
def move( group0 , group )
 faces=@faces.fetch(group)
 position=position( faces , group.transform )
 normal=face.normal.to_a
 origin=position
 rotation = Geom::Transformation.rotation( origin, Z_AXIS, -angle)
    vector = Geom::Vector3d.new(-origin.x,-origin.y,-origin.z )
    translation = Geom::Transformation.translation vector
  
end

# ------------------------------------------------------------------------------
# pop faces
# ------------------------------------------------------------------------------
def pop_faces( dir_ref )
 keys=@faces.keys
 dir=dir_ref.normalize
 #puts "dir_ref: #{dir}"
 for idx in 0..keys.length-1
 	gp=keys[idx]
 	transform=gp.transformation
    faces=@faces.fetch(gp)
    n=0
    #puts "gp: #{gp.name}"
    faces.each do | face |
     normal=transform*face.normal.normalize
     #puts "normal: #{normal}"
     if dir.dot(normal) > 0.5
       n=n+1
       #puts "dot: #{dir.dot(normal)}"
       @faces[gp]=face
       if n==2
        face.reverse!
         puts "gp: #{gp.name} - Reverse face: #{face}"
       end
     end
    end
    if n==0
    	face=faces[0]
    	@faces[gp]=face
    	face.reverse!
        puts "gp: #{gp.name} - Reverse face: #{face}"
    end	
 end
end


# ------------------------------------------------------------------------------
# pop faces
# ------------------------------------------------------------------------------
def vfaces_build( direction )
	puts "direction: #{direction}\n"
     @faces.each_pair do |group,face|
        vfaces=Hash.new
        transform=group.transformation
        #puts "group: #{group}\n"
        #puts "group: #{group.name}\n\n"
        face.loops.each do |loop|
             if loop.outer?
             	 pos=Geom::Point3d.new( position2_a( face , transform ))
                 planea=[pos, (transform*face.normal).cross(Geom::Vector3d.new( direction))]
                 planeb=[pos, transform*face.normal]
                 linea=Geom.intersect_plane_plane(planea, planeb)
                 #puts "linea: #{linea}\n"
                 loop.edges.each do |edge|
                     line = edge.line
                     line=[transform*line[0],transform*line[1]]
            	     pt=Geom.intersect_line_line(line, linea)
            	     #puts "line: #{line}\n"
            	     #puts "pt: #{pt}\n"
            	     if not pt.nil?
            	         vec=pos.vector_to( pt )
            	         angle=Geom::Vector3d.new(direction).angle_between( line[1] )
            	         #puts "angle: #{angle}\n"
            	         if ((angle - Math::PI/2 ).abs) < (Math::PI/2/4)
            	         	 vec1=Geom::Vector3d.new( pt, transform*edge.vertices[0].position)
            	             vec2=Geom::Vector3d.new( pt, transform*edge.vertices[1].position)
            	             #puts "vec1: #{vec1.length}\n"
            	             #puts "vec2: #{vec2.length}\n"
            	             #puts "edge: #{edge.length}\n"
            	             #puts "vec1.dot( vec2): #{vec1.dot(vec2)}\n\n"
            	         	 if vec1.dot( vec2) < 0
            	                 edge.faces.each do |f|
            	                    #puts "edge.faces.length: #{edge.faces.length}\n"
            		                if f!= face
            		                    vfaces[f]=direction.dot vec
            	                    end
            	                end
            	             end #common
            	         end
            	     end 
                end #edges
             end #if
        end #loops
        if vfaces.length > 0
            vfaces=vfaces.sort_by { |face,d| d }
            vfaces = Hash[vfaces.map {|key, value| [key, value]}]
            #puts "vfaces: #{vfaces}\n\n"
            @vfaces[group] = [ vfaces.keys.first , vfaces.keys.last]
            #puts " vfaces.keys.first.normal: #{ (transform*vfaces.keys.first.normal).to_a}\n"
            #puts " vfaces.keys.last.normal: #{  (transform*vfaces.keys.last.normal).to_a}\n"
            #puts "@vfaces[group]: #{@vfaces[group]}\n\n"
        end
     end #faces.each
end

# ------------------------------------------------------------------------------
# round_a
# ------------------------------------------------------------------------------
def  round_a( array )
	return array.map{|v| (v).round(4)}
end

# ------------------------------------------------------------------------------
# round_vec
# ------------------------------------------------------------------------------
def  round_vec( vec  )
	return Geom::Vector3d.new(round_a((vec).to_a))
end

# ------------------------------------------------------------------------------
# round_pt
# ------------------------------------------------------------------------------
def  round_pt( pt  )
	return Geom::Point3d.new(round_a((pt).to_a))
end

# ------------------------------------------------------------------------------
# round_line
# ------------------------------------------------------------------------------
def  round_line( line )
	return [round_pt(line[0]),round_vec(line[1])]
end

# ------------------------------------------------------------------------------
# a_to_mm
# ------------------------------------------------------------------------------
def a_to_mm( array )
  return array.map{|x| x.to_mm }
end

# ------------------------------------------------------------------------------
# vfaces_direction
# ------------------------------------------------------------------------------
def vfaces_direction
    keys=@vfaces.keys
    for idx in 0..keys.length-1
      group=keys[idx]
      transform=group.transformation
      vfaces=@vfaces[group]
      face1234=Hash.new
      nb1234=Hash.new
      puts "group=#{group}\n"
      puts "group=#{group.name}\n"
      puts "vfaces=#{vfaces}\n"
      vfaces.each do |face|
      	  normal=(transform*face.normal).to_a
      	  puts "normal=#{normal}\n"
      	  if not face1234[normal]
      	    face1234[normal]=position2_a( face , transform )
      	    nb1234[normal]=1
      	  else
      	    nb1234[normal]=nb1234[normal]+1
      	    pt1_a=face1234[normal]
      	    pt2_a=position2_a( face , transform )
      	    face1234[normal]=pt1_a.zip(pt2_a).map { |z| z.inject(&:+) }
      	  end
      end
      knb=nb1234.keys
      for id in 0..knb.length-1
      	  normal=knb[id]
      	  nb=nb1234[normal]
      	  if nb > 0
      	    pt1_a=face1234[normal]
      	    face1234[normal]=pt1_a.collect { |x| x / nb }
      	  end
      	  #puts "face1234[#{normal}] #{a_to_mm(face1234[normal])}\n\n"
      end
      @faces1234[group]=face1234
      puts "face1234=#{face1234}\n\n"
    end
end

# ------------------------------------------------------------------------------
# align
# ------------------------------------------------------------------------------
def align( model )
    #determine references
    if @faces.length < 2
     UI.messagebox "Selection must contain two or more groups or components."
     return
    end
    keys=@faces.keys
    group0=keys.find_all { |g| g.name == "0" }
    if group0.nil? or group0.length == 0
         UI.messagebox "No first group found. First group must be named 0."
         return
     else if group0.length > 1
         UI.messagebox "To many first groups with name 0."
         group0.each { |g| g.name = "" } 
         return
     end
    end
    
    # Group0
    group0=group0[0]
    faces0=@faces.fetch(group0)
    transform0=group0.transformation
    # Group1
    group1=keys.find { |gp| gp!=nil and gp != group0 }
    transform1=group1.transformation
    # Face_ref
    faces_ref=faces0.sort_by { |face| face.area }.reverse
    faces_ref_a=faces_ref.to_a
    puts "faces_ref_a.aera: #{(faces_ref_a.map{|face| face.area*(1).to_mm*(1).to_mm})}"
    puts "faces_ref_a.normal: #{(faces_ref_a.map{|face| face.normal})}"
    face_ref=faces_ref_a.find { |f| f!=nil }
    puts "face_ref: #{(face_ref)}"
    # Suppress one of the two twin faces
    pop_faces( (transform0*face_ref.normal).to_a )
    
    puts "@faces: #{@faces}"
    face0=@faces.fetch(group0)
    face1=@faces.fetch(group1)
    # Build vfaces
    pos0=Geom::Vector3d.new position2_a( face0 , transform0 )
    pos1=Geom::Vector3d.new position2_a( face1 , transform1 )
    dir=Geom::Vector3d.linear_combination(1, pos1, -1, pos0)
    vfaces_build dir.to_a
    # Build directions
    vfaces_direction
    #Reference
    @ref_position["Name"]=group0.name
    @ref_position["Position"]=position2_a( face_ref , transform0 )
    @ref_position["Normal"]=(transform0*face_ref.normal).to_a
    @ref_position["Direction"]=direction2_a( true , group0 , group1 )
    #puts "ref_position: #{@ref_position}\n\n"
    
    # Build all positions
    @positions[group0]=@ref_position
    pt0=Geom::Point3d.new @ref_position["Position"]
    distances=Hash.new
    for idx in 0..keys.length-1
      group=keys[idx]
      position=Hash.new
      if group != group0
        transform=group.transformation
        face=@faces[group]
        position["Name"]=group.name
        position["Position"]=position2_a( face , transform )
        position["Normal"]=(transform*face.normal).to_a
        position["Direction"]=direction2_a( false , group0 , group )
        @positions[group]=position
        pt1=Geom::Point3d.new position["Position"]
        vec= Geom::Vector3d.new( pt0, pt1 )
        distances[group]=vec.length
        position["Distance"]=vec.length
       end
    end
    
    puts "@positions: #{@positions}\n\n"
    #puts "distances: #{distances}\n\n"
    distances=distances.sort_by { |group,d| d }
    distances = Hash[distances.map {|key, value| [key, value]}]
    #puts "distances: #{distances}\n\n"
    
    # Transformations
    translation=Geom::Transformation.new
    pt0=Geom::Point3d.new( @ref_position["Direction"]["Front"]["Pos"] )
    distances.each_pair do |group,v|
            position=@positions[group]
            transform=group.transformation
            #puts "position: #{position}\n"
            # Rotations
            norm0 = Geom::Vector3d.new @ref_position["Normal"]
            dir0= Geom::Vector3d.new @ref_position["Direction"]["Front"]["Vec"]
            edge0=norm0.cross dir0
            norm1 = Geom::Vector3d.new position["Normal"]
            dir1= Geom::Vector3d.new position["Direction"]["Front"]["Vec"]
            pt1=Geom::Point3d.new position["Direction"]["Rear"]["Pos"]
            puts "pt0: #{pt0}\n"
            puts "pt1: #{pt1}\n"
            angle1 = norm0.angle_between(norm1)
            puts "angle1: #{angle1*180/Math::PI}\n"
            axe=norm0.cross(norm1)
            if axe.length != 0 and angle1 != 0
                 rot1 = Geom::Transformation.rotation(pt1, axe, -angle1)
            else
                 rot1 = Geom::Transformation.new
            end
            norm1=(dir0.cross(rot1*dir1)).normalize
            angle2 = (dir0).angle_between( rot1*dir1 )
            angle2=-angle2*norm0.dot(norm1)
            puts "angle2: #{angle2*180/Math::PI}\n"
            if angle2 != 0
                 rot2 = Geom::Transformation.rotation(pt1, norm0, angle2)
            else
                 rot2 = Geom::Transformation.new
            end
            
            #puts "norm1: #{norm1}\n"
            #puts "dir1: #{dir1}\n"
            #puts "edge1: #{edge1}\n"
            #Translation
            vec= Geom::Vector3d.new( pt1, pt0 )
            translation = translation*(Geom::Transformation.translation vec )
            pt0=Geom::Point3d.new position["Direction"]["Front"]["Pos"]
            puts "pt0: #{pt0}\n"
            #Performance
            group.transformation = rot2*rot1*transform
            group.transformation = translation*group.transformation
    end
end
end

# ------------------------------------------------------------------------------
# wikihouse_align
# ------------------------------------------------------------------------------
def self.wikihouse_align ( model , interactive )
  entities = model.active_entities
  selection = model.selection
  if selection.empty?
    if interactive
      reply = UI.messagebox "No objects selected. Export the entire model?", MB_OKCANCEL
      if reply != REPLY_OK
        return
      end
    end
  else
    entities = selection
  end
    WikiHouseAlign.new( model , entities )
end


# ------------------------------------------------------------------------------
# load_wikihouse_align
# ------------------------------------------------------------------------------
def self.load_wikihouse_align
  model = Sketchup.active_model

  # Exit if a model wasn't available.
  if not model
    show_wikihouse_error "You need to open a SketchUp model before it can be fabricated"
    return
  end

  # Initialise an attribute dictionary for custom metadata.
  attr = model.attribute_dictionary WIKIHOUSE_TITLE, true
  if attr.size == 0
    attr["spec"] = WIKIHOUSE_SPEC
  end

  # Exit if it's an unsaved model.
  model_path = model.path
  if model_path == ""
    UI.messagebox "You need to save the model before the cutting sheets can be generated"
    return
  end

  # Try and infer the model's filename.
  filename = model.title
  if filename == ""
    filename = "Untitled"
  end

  # Get the model's parent directory and generate the new filenames to save to.
  directory = File.dirname(model_path)

  # Union
  data = wikihouse_align model ,true
  if not data
    return
  end

end



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# CLASS: WikiHouseMetrics
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
class WikiHouseMetrics
	    attr_accessor :data
 def initialize(model , entities )
     # Initialise the default attribute values.
    @faces = Hash.new
    @groups = groups = Hash.new
    @pathes = Hash.new
    @orphans = orphans = Hash.new
    @root = model
    @todo = todo = []
    @data = []

    WikiHouseExtension.colors(model)

    # Set a loop counter variable and the default identity transformation.
    loop = 0
    transform = Geom::Transformation.new
    
    # Aggregate all the entities into the ``todo`` array.
    entities.each { |entity| todo << [entity, transform] }
    
    # Visit all component and group entities defined within the model and count
    # up all orphaned face entities.
    while todo.length != 0
      Sketchup.set_status_text WIKIHOUSE_DETECTION_STATUS[(loop/10) % 5]
      loop += 1
      entity, transform = todo.pop
      case entity.typename
      when "Group", "ComponentInstance"
      	@material=entity.material if @material.nil?
        @name=entity.name if @name.nil?
        #puts "---> material = #{@material.name}"
        WikiHouseExtension.visit_entities(model, entity, transform, @groups, @todo, @faces , @pathes, false )
      when "Face"
        if orphans[WIKIHOUSE_DUMMY_GROUP]
          orphans[WIKIHOUSE_DUMMY_GROUP] += 1
        else
          orphans[WIKIHOUSE_DUMMY_GROUP] = 1
        end
      end
    end
    
    # Metrics
    @data = metrics model
    #puts "@data: #{(@data)}"
 end
# ------------------------------------------------------------------------------
# metrics
# ------------------------------------------------------------------------------
 def metrics( model ) 	
 	data_all=[]
    # Suppress one of the two twin faces
    pop_faces( )
    @faces.each_pair do |group,face|
        #puts "group: #{(group.name)}"
        #puts "face: #{(face.typename)}"
        data=Hash.new
    	data["name"]=group.name
    	data["surface"]=(face.area/1000/1000*(1).to_mm*(1).to_mm).round(2)
    	sheet_depth=WikiHouseExtension.settings["sheet_depth"].to_mm
    	data["volume"]=(face.area/1000/1000/1000*sheet_depth*(1).to_mm*(1).to_mm).round(3)
    	len=0
    	face.outer_loop.edgeuses.each  {|edgeuse|  len = len + edgeuse.edge.length }
    	data["length"]=((len/1000).to_mm).round(1)
    	data["path"]=@pathes[group]
    	data["material"]=' '
    	data["material"]=group.material.name if not group.material.nil?
    	#puts "data: #{(data)}"
    	data_all << data
    end
    return data_all
 end
 
 
 # ------------------------------------------------------------------------------
# pop faces
# ------------------------------------------------------------------------------
def pop_faces( )
 keys=@faces.keys
 for idx in 0..keys.length-1
    faces=@faces.fetch(keys[idx])
    @faces[keys[idx]]=faces[0]
 end
end
end
# ------------------------------------------------------------------------------
# wikihouse_metrics
# ------------------------------------------------------------------------------
def self.wikihouse_metrics ( model , interactive )
  entities = model.active_entities
  selection = model.selection
  if selection.empty?
    if interactive
      reply = UI.messagebox "No objects selected. Export the entire model?", MB_OKCANCEL
      if reply != REPLY_OK
        return
      end
    end
  else
    entities = selection
  end
   metrics=WikiHouseMetrics.new model , entities
    #puts "metrics.data: #{(metrics.data)}"
    return metrics.data
end


# ------------------------------------------------------------------------------
# load_wikihouse_metrics
# ------------------------------------------------------------------------------
def self.load_wikihouse_metrics
  model = Sketchup.active_model

  # Exit if a model wasn't available.
  if not model
    show_wikihouse_error "You need to open a SketchUp model before it can be fabricated"
    return
  end

  # Initialise an attribute dictionary for custom metadata.
  attr = model.attribute_dictionary WIKIHOUSE_TITLE, true
  if attr.size == 0
    attr["spec"] = WIKIHOUSE_SPEC
  end

  # Exit if it's an unsaved model.
  model_path = model.path
  if model_path == ""
    UI.messagebox "You need to save the model before the cutting sheets can be generated"
    return
  end

  # Try and infer the model's filename.
  filename = model.title
  if filename == ""
    filename = "Untitled"
  end

  # Get the model's parent directory and generate the new filenames to save to.
  directory = File.dirname(model_path)

  # Metrics
  data_all = wikihouse_metrics model ,true
  
  if not data_all
    return
  end
  txt_filename = File.join(directory, filename + ".txt")
  surface_tot=0
  volume_tot=0
  length_tot=0
  # Save the TXT data to the file.
  txt="Name\tSurface(m2)\tVolume(m3)\tLength(m)\tPath\tMaterial\n"
  txt << "-----------------------------------------------------------------------------------\n"
  #puts "data_all: #{(data_all)}"
  data_all.each do |data|
      name=data["name"]
      surface=data["surface"]
      volume=data["volume"]
      length=data["length"]
      path=data["path"]
      material=data["material"]
      surface_tot = surface_tot +surface
      volume_tot = volume_tot + volume
      length_tot = length_tot + length
      txt << "#{name}\t#{surface}\t#{volume}\t#{length}\t#{path}\t#{material}\n"
  end
  txt << "-----------------------------------------------------------------------------------\n"
  txt << "TOTALS\t#{surface_tot.round(2)}\t#{volume_tot.round(3)}\t#{length_tot.round(1)}\n"
  
  File.open(txt_filename, "wb") do |io|
        io.write txt
  end
  dialog = UI::WebDialog.new "Metrics Preview", true, "#{WIKIHOUSE_TITLE}-Preview", 800, 800, 150, 150, true
  dialog.set_file txt_filename
  if WIKIHOUSE_MAC
    dialog.show_modal
  else
    dialog.show
  end
end



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# CLASS: WikiHouseLabels
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
class WikiHouseLabels
 def initialize(model , entities )
     # Initialise the default attribute values.
    @faces = Hash.new
     @pathes = Hash.new
    @groups = groups = Hash.new
    @orphans = orphans = Hash.new
    @root = model
    @todo = todo = []

    WikiHouseExtension.colors(model)

    # Set a loop counter variable and the default identity transformation.
    loop = 0
    transform = Geom::Transformation.new
    
    # Aggregate all the entities into the ``todo`` array.
    entities.each { |entity| todo << [entity, transform] }
    
    # Visit all component and group entities defined within the model and count
    # up all orphaned face entities.
    while todo.length != 0
      Sketchup.set_status_text WIKIHOUSE_DETECTION_STATUS[(loop/10) % 5]
      loop += 1
      entity, transform = todo.pop
      case entity.typename
      when "Group", "ComponentInstance"
      	@material=entity.material if @material.nil?
        @name=entity.name if @name.nil?
        #puts "---> material = #{@material.name}"
        WikiHouseExtension.visit_entities(model, entity, transform, @groups, @todo, @faces, @pathes, false )
      when "Face"
        if orphans[WIKIHOUSE_DUMMY_GROUP]
          orphans[WIKIHOUSE_DUMMY_GROUP] += 1
        else
          orphans[WIKIHOUSE_DUMMY_GROUP] = 1
        end
      end
    end
    
    # Labels
    labels model
 end
 
# ------------------------------------------------------------------------------
# labels
# ------------------------------------------------------------------------------
 def labels( model )
 	list_gps=Hash.new
    @faces.each_pair do |group,face|
      layer=group.layer
      if list_gps[layer].nil?
      	 list_gps[layer]=1
      else
      	  list_gps[layer]=list_gps[layer]+1
      end
      id=list_gps[layer]
      len=layer.name.length
      if len > 6
        group.name=layer.name[0...3]+layer.name[len-3...len]+"##{id}"
      else
        group.name=layer.name+"##{id}"
      end
      puts "group.name: #{group.name}"
    end
 end
end #class


# ------------------------------------------------------------------------------
# wikihouse_labels
# ------------------------------------------------------------------------------
def self.wikihouse_labels ( model , interactive )
  entities = model.active_entities
  selection = model.selection
  if selection.empty?
    if interactive
      reply = UI.messagebox "No objects selected. Export the entire model?", MB_OKCANCEL
      if reply != REPLY_OK
        return
      end
    end
  else
    entities = selection
  end
  WikiHouseLabels.new model , entities
end


# ------------------------------------------------------------------------------
# load_wikihouse_labels
# ------------------------------------------------------------------------------
def self.load_wikihouse_labels
  model = Sketchup.active_model

  # Exit if a model wasn't available.
  if not model
    show_wikihouse_error "You need to open a SketchUp model before it can be fabricated"
    return
  end

  # Initialise an attribute dictionary for custom metadata.
  attr = model.attribute_dictionary WIKIHOUSE_TITLE, true
  if attr.size == 0
    attr["spec"] = WIKIHOUSE_SPEC
  end

  # Exit if it's an unsaved model.
  model_path = model.path
  if model_path == ""
    UI.messagebox "You need to save the model before the cutting sheets can be generated"
    return
  end
  
  # Labels
  wikihouse_labels model ,true
end

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# CLASS: WikiHouseUnique
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
class WikiHouseUnique
 def initialize(model , entities )
     # Initialise the default attribute values.
    @faces = Hash.new
     @pathes = Hash.new
    @groups = groups = Hash.new
    @orphans = orphans = Hash.new
    @root = model
    @todo = todo = []

    WikiHouseExtension.colors(model)

    # Set a loop counter variable and the default identity transformation.
    loop = 0
    transform = Geom::Transformation.new
    
    # Aggregate all the entities into the ``todo`` array.
    entities.each { |entity| todo << [entity, transform] }
    
    # Visit all component and group entities defined within the model and count
    # up all orphaned face entities.
    while todo.length != 0
      Sketchup.set_status_text WIKIHOUSE_DETECTION_STATUS[(loop/10) % 5]
      loop += 1
      entity, transform = todo.pop
      case entity.typename
      when "Group", "ComponentInstance"
      	@material=entity.material if @material.nil?
        @name=entity.name if @name.nil?
        #puts "---> material = #{@material.name}"
        WikiHouseExtension.visit_entities(model, entity, transform, @groups, @todo, @faces, @pathes, true )
      when "Face"
        if orphans[WIKIHOUSE_DUMMY_GROUP]
          orphans[WIKIHOUSE_DUMMY_GROUP] += 1
        else
          orphans[WIKIHOUSE_DUMMY_GROUP] = 1
        end
      end
    end
    
    # Unique
    unique model
 end
 
# ------------------------------------------------------------------------------
# unique
# ------------------------------------------------------------------------------
 def unique( model )
    @faces.each_pair do |group,face|
      if group.typename == "ComponentInstance"
          puts "Unique: #{@pathes[group]}\n"
          puts "Component: #{group.definition.name}\n"
          puts "Name: #{group.name}\n"
          group.make_unique
      end
    end
 end
end #class


# ------------------------------------------------------------------------------
# wikihouse_unique
# ------------------------------------------------------------------------------
def self.wikihouse_unique ( model , interactive )
  entities = model.active_entities
  selection = model.selection
  if selection.empty?
    if interactive
      reply = UI.messagebox "No objects selected. Export the entire model?", MB_OKCANCEL
      if reply != REPLY_OK
        return
      end
    end
  else
    entities = selection
  end
  reply = UI.messagebox "ATTENTION: all components will be unique ?", MB_OKCANCEL
  if reply != REPLY_OK
        return
  end
  WikiHouseUnique.new model , entities
end


# ------------------------------------------------------------------------------
# load_wikihouse_unique
# ------------------------------------------------------------------------------
def self.load_wikihouse_unique
  model = Sketchup.active_model

  # Exit if a model wasn't available.
  if not model
    show_wikihouse_error "You need to open a SketchUp model before it can be fabricated"
    return
  end

  # Initialise an attribute dictionary for custom metadata.
  attr = model.attribute_dictionary WIKIHOUSE_TITLE, true
  if attr.size == 0
    attr["spec"] = WIKIHOUSE_SPEC
  end

  # Exit if it's an unsaved model.
  model_path = model.path
  if model_path == ""
    UI.messagebox "You need to save the model before the cutting sheets can be generated"
    return
  end
  
  # Labels
  wikihouse_unique model ,true
end


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
# Select
# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
def self.Select( name )
     # Initialise the default attribute values.
     model = Sketchup.active_model
    entities = model.active_entities
    selection = model.selection
    if selection.empty?
      reply = UI.messagebox "No objects selected. Export the entire model?", MB_OKCANCEL
      if reply != REPLY_OK
        return
      end
    else
      entities = selection
    end
    @faces = Hash.new
    @pathes = Hash.new
    @groups = groups = Hash.new
    @orphans = orphans = Hash.new
    @root = model
    @todo = todo = []

    WikiHouseExtension.colors(model)

    # Set a loop counter variable and the default identity transformation.
    loop = 0
    transform = Geom::Transformation.new
    
    # Aggregate all the entities into the ``todo`` array.
    entities.each { |entity| todo << [entity, transform] }
    
    # Visit all component and group entities defined within the model and count
    # up all orphaned face entities.
    while todo.length != 0
      Sketchup.set_status_text WIKIHOUSE_DETECTION_STATUS[(loop/10) % 5]
      loop += 1
      entity, transform = todo.pop
      case entity.typename
      when "Group", "ComponentInstance"
      	@material=entity.material if @material.nil?
        @name=entity.name if @name.nil?
        #puts "---> material = #{@material.name}"
        WikiHouseExtension.visit_entities(model, entity, transform, @groups, @todo, @faces, @pathes , false )
      when "Face"
        if orphans[WIKIHOUSE_DUMMY_GROUP]
          orphans[WIKIHOUSE_DUMMY_GROUP] += 1
        else
          orphans[WIKIHOUSE_DUMMY_GROUP] = 1
        end
      end
    end
    
    # Select
    model.selection.clear
    @faces.each_pair do |group,face|
      layer=group.layer
      case group.typename
        when "Group"
          if (not group.name.nil?) and (group.name == name)
             puts "Layer: #{group.layer.name} - #{@pathes[group]}\n"
             group.visible=true
             model.selection.add group
          end
        when "ComponentInstance"
          if ((not group.definition.name.nil?) and (group.definition.name == name))or((not group.name.nil?) and (group.name == name))
             puts "Layer: #{group.layer.name} - #{@pathes[group]}\n"
             group.visible=true
             model.selection.add group
          end
      end                       
    end
    return
end

# ------------------------------------------------------------------------------
# Set Globals
# ------------------------------------------------------------------------------

if not file_loaded? __FILE__

  WIKIHOUSE_DIR = File.join File.dirname(__FILE__), "wikihouse/assets"

  WIKIHOUSE_MAKE = UI::Command.new "Make This House..." do
    load_wikihouse_make
  end

  WIKIHOUSE_MAKE.tooltip = "Convert a model of a House into printable components"
  WIKIHOUSE_MAKE.small_icon = File.join WIKIHOUSE_DIR, "make-16.png"
  WIKIHOUSE_MAKE.large_icon = File.join WIKIHOUSE_DIR, "make.png"
  WIKIHOUSE_MAKE.set_validation_proc {
    if Sketchup.active_model
      MF_ENABLED
    else
      MF_DISABLED|MF_GRAYED
    end
  }

# ------------------------------------------------------------------------------
# Union
# ------------------------------------------------------------------------------
   WIKIHOUSE_UNION = UI::Command.new "Union components..." do
    load_wikihouse_union
  end


  WIKIHOUSE_UNION.tooltip = "Union components"
  WIKIHOUSE_UNION.small_icon = File.join WIKIHOUSE_DIR, "union-16.png"
  WIKIHOUSE_UNION.large_icon = File.join WIKIHOUSE_DIR, "union.png"
  WIKIHOUSE_UNION.set_validation_proc {
    if Sketchup.active_model
      MF_ENABLED
    else
      MF_DISABLED|MF_GRAYED
    end
  }
  
  
# ------------------------------------------------------------------------------
# Align
# ------------------------------------------------------------------------------
   WIKIHOUSE_ALIGN = UI::Command.new "Align components..." do
    load_wikihouse_align
  end
  
  WIKIHOUSE_ALIGN.tooltip = "Align components"
  WIKIHOUSE_ALIGN.small_icon = File.join WIKIHOUSE_DIR, "align-16.png"
  WIKIHOUSE_ALIGN.large_icon = File.join WIKIHOUSE_DIR, "align.png"
  WIKIHOUSE_ALIGN.set_validation_proc {
    if Sketchup.active_model
      MF_ENABLED
    else
      MF_DISABLED|MF_GRAYED
    end
  }
  
  
# ------------------------------------------------------------------------------
# Metrics
# ------------------------------------------------------------------------------
   WIKIHOUSE_METRICS = UI::Command.new "Calculate metrics..." do
    load_wikihouse_metrics
  end
  
  WIKIHOUSE_METRICS.tooltip = "Calculate metrics"
  WIKIHOUSE_METRICS.small_icon = File.join WIKIHOUSE_DIR, "metrics-16.png"
  WIKIHOUSE_METRICS.large_icon = File.join WIKIHOUSE_DIR, "metrics.png"
  WIKIHOUSE_METRICS.set_validation_proc {
    if Sketchup.active_model
      MF_ENABLED
    else
      MF_DISABLED|MF_GRAYED
    end
  }
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Labels
# ------------------------------------------------------------------------------
   WIKIHOUSE_LABELS = UI::Command.new "Labels..." do
    load_wikihouse_labels
  end
  
  WIKIHOUSE_LABELS.tooltip = "Labels"
  WIKIHOUSE_LABELS.small_icon = File.join WIKIHOUSE_DIR, "labels-16.png"
  WIKIHOUSE_LABELS.large_icon = File.join WIKIHOUSE_DIR, "labels.png"
  WIKIHOUSE_LABELS.set_validation_proc {
    if Sketchup.active_model
      MF_ENABLED
    else
      MF_DISABLED|MF_GRAYED
    end
  }
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Unique
# ------------------------------------------------------------------------------
   WIKIHOUSE_UNIQUE = UI::Command.new "Unique..." do
    load_wikihouse_unique
  end
  
  WIKIHOUSE_UNIQUE.tooltip = "Unique"
  WIKIHOUSE_UNIQUE.small_icon = File.join WIKIHOUSE_DIR, "unique-16.png"
  WIKIHOUSE_UNIQUE.large_icon = File.join WIKIHOUSE_DIR, "unique.png"
  WIKIHOUSE_UNIQUE.set_validation_proc {
    if Sketchup.active_model
      MF_ENABLED
    else
      MF_DISABLED|MF_GRAYED
    end
  }
  
# ------------------------------------------------------------------------------


  WIKIHOUSE_SETTINGS = UI::Command.new 'Settings...' do
      load_wikihouse_settings
  end

    WIKIHOUSE_SETTINGS.tooltip = "Change #{WIKIHOUSE_TITLE} settings"
    WIKIHOUSE_SETTINGS.small_icon = File.join(WIKIHOUSE_DIR, 'cog-16.png')
    WIKIHOUSE_SETTINGS.large_icon = File.join(WIKIHOUSE_DIR, 'cog.png')
    WIKIHOUSE_SETTINGS.set_validation_proc {
      MF_ENABLED
    }
  
  # Register a new toolbar with the commands.
  WIKIHOUSE_TOOLBAR = UI::Toolbar.new WIKIHOUSE_TITLE
  WIKIHOUSE_TOOLBAR.add_item WIKIHOUSE_MAKE
  WIKIHOUSE_TOOLBAR.add_item WIKIHOUSE_UNION
  WIKIHOUSE_TOOLBAR.add_item WIKIHOUSE_ALIGN
  WIKIHOUSE_TOOLBAR.add_item WIKIHOUSE_METRICS
  WIKIHOUSE_TOOLBAR.add_item WIKIHOUSE_LABELS
  WIKIHOUSE_TOOLBAR.add_item WIKIHOUSE_UNIQUE
  WIKIHOUSE_TOOLBAR.add_item(WIKIHOUSE_SETTINGS)
  WIKIHOUSE_TOOLBAR.show

  # Register a new submenu of the standard Plugins menu with the commands.
  WIKIHOUSE_MENU = UI.menu("Plugins").add_submenu WIKIHOUSE_TITLE
  WIKIHOUSE_MENU.add_item WIKIHOUSE_MAKE
  WIKIHOUSE_MENU.add_item WIKIHOUSE_UNION
  WIKIHOUSE_MENU.add_item WIKIHOUSE_ALIGN
  WIKIHOUSE_MENU.add_item WIKIHOUSE_METRICS
  WIKIHOUSE_MENU.add_item WIKIHOUSE_LABELS
  WIKIHOUSE_MENU.add_item WIKIHOUSE_UNIQUE
  WIKIHOUSE_MENU.add_item(WIKIHOUSE_SETTINGS)

  # Add our custom AppObserver.
  Sketchup.add_observer WikiHouseAppObserver.new

  # Display the Ruby Console in dev mode.
  if WIKIHOUSE_DEV
    Sketchup.send_action "showRubyPanel:"
    def w
      load "wikihouse.rb"
    end
    puts ""
    puts "#{WIKIHOUSE_TITLE} Plugin Successfully Loaded."
    puts ""
  end

  file_loaded __FILE__

end

def self.w
  load "wikihouse.rb"
  puts
  data = make_wikihouse Sketchup.active_model, false
  if data
    filename = "/Users/tav/Documents/sketchup/Wikhouse10_tester3.svg"
    svg_data, dxf_data = data
    # Save the SVG data to the file.
    File.open(filename, "wb") do |io|
      io.write svg_data
    end
    "Sheets generated!"
  end
end
end

  
