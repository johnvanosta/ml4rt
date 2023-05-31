#Function to convert projected to geographic coordinates
import utm

#Function to convert geographic to projected coordinates
def to_latlon(easting, northing, zone_number, letter):
    latitude, longitude = utm.to_latlon(easting, northing, zone_number, letter)
    return latitude, longitude