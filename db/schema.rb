# This file is auto-generated from the current state of the database. Instead
# of editing this file, please use the migrations feature of Active Record to
# incrementally modify your database, and then regenerate this schema definition.
#
# Note that this schema.rb definition is the authoritative source for your
# database schema. If you need to create the application database on another
# system, you should be using db:schema:load, not running all the migrations
# from scratch. The latter is a flawed and unsustainable approach (the more migrations
# you'll amass, the slower it'll run and the greater likelihood for issues).
#
# It's strongly recommended to check this file into your version control system.

ActiveRecord::Schema.define(:version => 20110528225934) do

  create_table "characters", :force => true do |t|
    t.integer  "user_id"
    t.string   "name"
    t.integer  "gold"
    t.string   "inventory"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "cities", :force => true do |t|
    t.string   "name"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "districts", :force => true do |t|
    t.integer  "city_id"
    t.string   "name"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "items", :force => true do |t|
    t.string   "type"
    t.string   "name"
    t.string   "icon"
    t.integer  "cost"
    t.integer  "weight"
    t.integer  "space"
    t.text     "description"
    t.datetime "created_at"
    t.datetime "updated_at"
    t.integer  "damage_base"
    t.integer  "damage_range"
    t.string   "damage_type"
    t.integer  "strength"
    t.integer  "control"
    t.integer  "fire_damage_base"
    t.integer  "fire_damage_range"
    t.string   "rate_of_fire"
    t.string   "clip_type"
    t.string   "ammo_type"
    t.string   "coverage"
    t.integer  "puncture_resistance"
    t.integer  "sunder_resistance"
    t.integer  "rupture_resistance"
    t.integer  "psionic_resistance"
    t.integer  "temperature_resistance"
    t.integer  "flexibility"
    t.integer  "comfort"
    t.string   "tool_type"
    t.integer  "enhancement"
  end

  create_table "point_of_interests", :force => true do |t|
    t.integer  "district_id"
    t.string   "name"
    t.string   "stocks"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

  create_table "users", :force => true do |t|
    t.string   "login"
    t.string   "passcode"
    t.string   "salt"
    t.datetime "created_at"
    t.datetime "updated_at"
  end

end
