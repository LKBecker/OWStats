import datetime
import itertools
import json
from statistics import mean, median
import sys      
import urllib.error
import urllib.parse
import urllib.request
import time
import os


BALANCE_TOLERANCE = 100
SR_FOR_UNRANKED = 2000
DEFAULT_SERVER = 'http://owapi.net/api/v3/u/'
DEFAULT_WAIT_BETWEEN_REQS = 30
VERSION="0.0.1"
HEROES = ["Doomfist", "Genji", "McCree", "Pharah", "Reaper", "Soldier76", "Sombra", "Tracer", "Bastion", "Hanzo", "Junkrat", "Mei", "Torbjorn", "Widowmaker", "DVa", "Orisa", "Reinhardt", "Roadhog", "Winston", "Zarya", "Ana", "Brigitte", "Lucio", "Mercy", "Moira", "Symmetra", "Zenyatta"]

#Stack2 = [ "Eraserbrain-1563", "XxWEEDGOKUxX-1418", "Apologies-11703" ]
Stack2 = [ "LGBTracer-2875", "Wonderbrah-21110" ]


class OWAPIClient():
	def __init__(this, server=DEFAULT_SERVER, req_stagger=DEFAULT_WAIT_BETWEEN_REQS):
		this.server = server
		this.req_stagger = req_stagger
		this.last_req = 0

	def get_data(this, user, returnData=False):
		# check if we need to rate limit ourselves
		runtime = datetime.datetime.now().strftime("%y%m%d")
		jsonfile = "./{}/{}_{}.json".format(runtime, runtime, user)
		if (os.path.isdir("./" + runtime)):
			if(os.path.isfile(jsonfile)):
				sys.stdout.write('Player {0} already exists today, loading file...\n'.format(user))
				with open(jsonfile, 'r') as io:
					content = json.loads(io.read())
				return content
		
		delta = time.time() - this.last_req
		if delta < 1: time.sleep(1 - delta)		     #sleep until limit normal again
		this.last_req = time.time()
		try:
			print("Downloading current stats for " + user + "...")
			s_user = urllib.parse.quote(user)
			req = urllib.request.Request(this.server + s_user + '/blob')
			req.add_header('User-Agent', 'Mozilla/5.0 (X11; U; Linux i686) Gecko/20071127 Firefox/2.0.0.11')
			response = urllib.request.urlopen(req)
			content = response.read().decode('utf-8')
			if content:
				try: os.mkdir("./" + runtime)
				except: pass
				outfile = open(jsonfile, 'w')
				outfile.write(content)
				outfile.close()
				if returnData:
					return content
				else:
					return

		except urllib.error.HTTPError as e: # check if we are being rate limited by the server
			if e.code == 429:
				if 'Retry-After' in e.headers:
					retry = float(e.headers['Retry-After'])
					print("Too many requests. Advised to retry in " + str(retry) + " seconds")
					time.sleep(retry + 1)
					this.get_data(user)     #recurse this after specified retry period

			if e.code == 404:
				sys.stderr.write('Player {0} does not appear to exist. Please check and retry.\n'.format(user))
			else:
				sys.stderr.write('Request failed for {0} ({2}{0}/blob): {1.code} {1.reason}\n'.format(user, e, this.server))

class TeamMember():
	def __init__(this, BNetTag, DataClump):
		this.Tag = BNetTag
		this.Name = this.Tag.split("-")[0]
		
		#check if EU stats AND US stats
		try:
			this.SR = DataClump['eu']['stats']['competitive']['overall_stats']['comprank']
			if (this.SR is None): 
				this._SR = SR_FOR_UNRANKED
				this.SR = "No data"
			else:
				this._SR = this.SR
		except:
			this._SR = SR_FOR_UNRANKED
			this.SR = "Parse Error"
		#if (DataClump['us']!=None): EU_SR = DataClump['eu']['competitive']['overall_stats']['comprank']
		# try:
			# if ('competitive' in DataClump['eu']['heroes']['playtime'].keys()):
				# this.PlayTimesComp = DataClump['eu']['heroes']['playtime']['competitive']
				#this.PlayTimesCompTotal = sum(this.PlayTimesComp.values)
			# if ('quickplay' in DataClump['eu']['heroes']['playtime'].keys()):
				# this.PlayTimesQP = DataClump['eu']['heroes']['playtime']['quickplay']
				#this.PlayTimesQPTotal = sum(this.PlayTimesQP.values)
		# except: pass
				
def BalanceTeams(Players, TeamSize = 6):
	#get NA and EU SRs, use higher one
	PlayerData = []
	for Player in Players:
		data = client.get_data(Player)
		PlayerInstance = TeamMember(Player, data)
		PlayerData.append(PlayerInstance)
	PlayerData.sort(key = lambda x: x._SR)
	Team1 = PlayerData[1::2] #give them every second player in PlayerData, starting at 1
	Team2 = PlayerData[0::2] #give them every second player in PlayerData, starting at 0
	print("\n\n")
	print("Team 1 | Mean SR {0}, Median SR {1} | {2}\n".format(mean(map(lambda x: x._SR, Team1)), median(map(lambda x: x._SR, Team1)), list(map(lambda x: "{0}({1})".format(x.Name, x.SR), Team1))))
	print("Team 2 | Mean SR {0}, Median SR {1} | {2}\n".format(mean(map(lambda x: x._SR, Team2)), median(map(lambda x: x._SR, Team2)), list(map(lambda x: "{0}({1})".format(x.Name, x.SR), Team2))))
	t1=list(map(lambda x: "{0}\t{1}\t\t".format(x.Name, x.SR), Team1))	
	t2=list(map(lambda x: "{0}\t{1}\n".format(x.Name, x.SR), Team2))	
	Team = "".join(list(map(lambda x: "".join(x), itertools.zip_longest(t1, t2, fillvalue=""))))
	print(Team)
	with open("{}-Teams.txt".format(datetime.datetime.now().strftime("%y%m%d-%H%M")), 'w') as io:
		io.write("Team1\t\t\tTeam2\nPlayer\tSR\t\tPlayer\tSR\n"+Team)
					
if __name__ == "__main__":
	global client 
	client = OWAPIClient()
	#for Player in AllPlayers: client.get_data(Player)
	#for Player in CompStack1: client.get_data(Player)
	#client.get_data("ÐickKickem-1982")
	#client.get_data("DorialD-2401")
	#client.get_data("LGBTracer-2875")
	#client.get_data("Wonderbrah-21110")
	#data=BalanceTeams(Stack2)
	for Player in Stack2: client.get_data(Player)
