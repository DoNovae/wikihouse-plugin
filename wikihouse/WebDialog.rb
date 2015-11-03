# Web Dialogues

module WikiHouseExtension


  # ------------------------------------------------------------------------------
  # Settings Web Dialogue
  # ------------------------------------------------------------------------------

  def self.load_wikihouse_settings

    # Create WebDialog
    dialog = UI::WebDialog.new("#{WIKIHOUSE_TITLE} - Settings",
      true, "#{WIKIHOUSE_TITLE}-Settings", 480, 750, 150, 150, true)

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
          if k=="scale"
            dims[k] = v
          else
            dims[k] = v.to_mm
          end
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
         if k=="scale"
            settings[k] = v
          else
            settings[k] = (v.mm).to_inch
          end
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