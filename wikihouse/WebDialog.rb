# Web Dialogues

module WikiHouseExtension

  # ------------------------------------------------------------------------------
  # Common Callbacks
  # ------------------------------------------------------------------------------
  
  # Download Callback
  # -----------------
  def self.wikihouse_download_callback(dialog, params)
    # Exit if the download parameters weren't set.
    if params == ""
      show_wikihouse_error "Couldn't find the #{WIKIHOUSE_TITLE} model name and url"
      return
    end

    is_comp, base64_url, blob_url, name = params.split ",", 4
    model = Sketchup.active_model

    # Try and save the model/component directly into the current model.
    if model and is_comp == '1'
      reply = UI.messagebox("Load this directly into your SketchUp model?", MB_YESNOCANCEL)
      if reply == IDYES
        loader = WikiHouseLoader.new name
        blob_url = WIKIHOUSE_SERVER + blob_url
        model.definitions.load_from_url blob_url, loader
        if not loader.error
          dialog.close
          UI.messagebox "Successfully downloaded #{name}"
          component = model.definitions[-1]
          if component
            model.place_component component
          end
          return
        else
          UI.messagebox loader.error
          reply = UI.messagebox "Would you like to save the model file instead?", MB_YESNO
          if reply == IDNO
            return
          end
        end
      elsif reply == IDNO
        # Skip through to saving the file directly.
      else
        return
      end
    end

    # Otherwise, get the filename to save into.
    filename = UI.savepanel "Save Model", WIKIHOUSE_SAVE, "#{name}.skp"
    if not filename
      show_wikihouse_error "No filename specified to save the #{WIKIHOUSE_TITLE} model. Please try again."
      return
    end

    # TODO(tav): Ensure that this is atomic and free of thread-related
    # concurrency issues.
    WikiHouseExtension.downloads_id += 1
    download_id = WikiHouseExtension.downloads_id.to_s

    WIKIHOUSE_DOWNLOADS[download_id] = filename

    # Initiate the download.
    dialog.execute_script "wikihouse.download('#{download_id}', '#{base64_url}');"
  end

  # Save Callback
  # -------------
  def self.wikihouse_save_callback(dialog, download_id)
    errmsg = "Couldn't find the #{WIKIHOUSE_TITLE} model data to save"

    # Exit if the save parameters weren't set.
    if download_id == ""
      show_wikihouse_error errmsg
      return
    end

    if not WIKIHOUSE_DOWNLOADS.key? download_id
      show_wikihouse_error errmsg
      return
    end

    filename = WIKIHOUSE_DOWNLOADS[download_id]
    WIKIHOUSE_DOWNLOADS.delete download_id

    segment_count = dialog.get_element_value "design-download-data"
    dialog.close

    if segment_count == ""
      show_wikihouse_error errmsg
      return
    end

    data = []
    for i in 0...segment_count.to_i
      segment = dialog.get_element_value "design-download-data-#{i}"
      if segment == ""
        show_wikihouse_error errmsg
        return
      end
      data << segment
    end

    # Decode the base64-encoded data.
    data = data.join('').unpack("m")[0]
    if data == ""
      show_wikihouse_error errmsg
      return
    end

    # Save the data to the local file.
    File.open(filename, 'wb') do |io|
      io.write data
    end

    reply = UI.messagebox "Successfully saved #{WIKIHOUSE_TITLE} model. Would you like to open it?", MB_YESNO
    if reply == IDYES
      if not Sketchup.open_file filename
        show_wikihouse_error "Couldn't open #{filename}"
      end
    end
  end

  # Error Callback
  # --------------
  def self.wikihouse_error_callback(dialog, download_id)
    if not WIKIHOUSE_DOWNLOADS.key? download_id
      return
    end

    filename = WIKIHOUSE_DOWNLOADS[download_id]
    WIKIHOUSE_DOWNLOADS.delete download_id

    show_wikihouse_error "Couldn't download #{filename} from #{WIKIHOUSE_TITLE}. Please try again."
  end

 
  
  
  # ------------------------------------------------------------------------------
  # Settings Web Dialogue
  # ------------------------------------------------------------------------------

  def self.load_wikihouse_settings

    # Create WebDialog
    dialog = UI::WebDialog.new("#{WIKIHOUSE_TITLE} - Settings",
      true, "#{WIKIHOUSE_TITLE}-Settings", 480, 600, 150, 150, true)

    # Get Current WikiHouse Settings
    dialog.add_action_callback('fetch_settings') { |d, args|

      if args == 'default'
        settings = DEFAULT_SETTINGS
      elsif args == 'current'
        settings = WikiHouseExtension.settings
      end

      if args == 'default' || args == 'current'
        # Convert Dimenstions to mm
        dims = {}
        for k, v in settings do
          dims[k] = v.to_mm
        end
        script = "recieve_wikihouse_settings('#{JSON.to_json(dims)}');"
        d.execute_script(script)
      end

    }

    # Set Web Dialog's Callbacks
    dialog.add_action_callback("update_settings") { |d, args|

      close_flag = false
      if args.include? "--close"
        close_flag = true
        args = args.gsub("--close", "")
      end

      # UI.messagebox("Passed Arguments = #{args}")
      settings = WikiHouseExtension.settings
      new_settings = JSON.from_json(args)

      for k,v in new_settings do
        # Convert mm back to inches
        #puts "settings[k]: #{v.mm}"
        settings[k] = (v.mm).to_inch
        #puts "settings[k]: #{settings[k]}"
      end

      # Recalculate inner heights and widths
      settings["sheet_inner_height"] = settings["sheet_height"] - (2 * settings["margin"])
      settings["sheet_inner_width"] = settings["sheet_width"] - (2 * settings["margin"])

      puts "Dimensions Updated!"

      if close_flag == true
        d.close
      else
        d.execute_script("display_status('" + "Settings Updated!" + "');")
      end
    }

    # Cancel and close dialog
    dialog.add_action_callback("cancel_settings") { |d, args|
      d.close }

    # Set HTML
    html_path = File.join(WEBDIALOG_PATH, 'settings.html')
    dialog.set_file(html_path)
    if WIKIHOUSE_MAC
      dialog.show_modal
    else
      dialog.show
    end

    puts "Dialog Loaded"

  end

end