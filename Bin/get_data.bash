###############################################################################
# Author: Pablo Reyes Moctezuma <info@pabloreyes.com.mx>
# Objective: Download Our World in Data Covid data
###############################################################################

# Define functions ============================================================

# Function to log a message to the stdout
# Arg1: Level
# Arg2: Msg
log_msg() {
    # Define locals
    local LEVEL=${1:="INFO"}
    local MSG=${2:=""}
    # Echo msg
    echo "$(date) | $LEVEL : $MSG"
}

# Main routine ================================================================

# Download data
# Flag -O (output file)

log_msg "INFO" "Getting COVID data..."

wget https://covid.ourworldindata.org/data/owid-covid-data.csv -O Data/covid-data.csv

log_msg "INFO" "Data downloaded!"

# Compress data

log_msg "INFO" "Compressing data..."

tar -cfz Data/covid-data-compressed.tgz Data/covid-data.csv

log_msg "INFO" "Data compressed!"