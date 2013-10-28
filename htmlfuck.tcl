#    Collection of eggdrop TCL scripts
#    Copyright (C) 2013  Desislav Ivanov 

#    This program is free software: you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation, either version 3 of the License, or
#    (at your option) any later version.

#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.

#    You should have received a copy of the GNU General Public License
#    along with this program.  If not, see [http://www.gnu.org/licenses/].

set ns "htmlfuck"
catch ${ns}::uninstall
namespace eval $ns {
	unset ::ns
	bind evnt - prerehash [namespace current]::uninstall
	bind join - * [namespace current]::joining
	bind mode - * [namespace current]::modechange
	
	variable wasenforce
	
	array set wasenforce {}
	
	proc isvalidhtml { nickname {hostmask ""} } {
		if {![expr {$hostmask ne ""}]} {
			set hostmask [getchanhost $nickname]
		}
		return [expr {[regexp -nocase {([a-f0-9][a-f0-9])+@.*\.html\.chat} $hostmask] || [regexp -nocase {([a-f0-9][a-f0-9])+@.*\.mibbit\.com} $hostmask]}]
	}
	
	proc ishtml { nickname {hostmask "" } } {
		if {![expr {$hostmask ne ""}]} {
			set hostmask [getchanhost $nickname]
		}
		return [expr {[matchstr "*@*.html.chat" $hostmask] || [matchstr "*@*.mibbit.com" $hostmask]}]
	}
	
	proc modechange {nick uhost hand chan mode {target ""}} {
		variable wasenforce
		if { $mode == "+b"} {
			if {[matchaddr $target "invalid-!CJCJCJCJCJ_CJCJ@blq_blq.html.chat"] || [matchaddr $target "invalid-!CJCJCJC_JCJCJCJ@blq_blq.mibbit.com"]} {
				set wasenforce($chan) [channel get $chan enforcebans]
				putlog "Detected wide ban on html chat from $nick for mask $target in $chan. Removing it..."
				if {$wasenforce($chan)} {
					putlog "Setting -enforcebans on $chan ."
					channel set $chan -enforcebans
				}
				putserv "NOTICE $nick :Don't ban the whole webchat in $chan stupido !"
				putquick "MODE $chan -b $target" -next
			} else {
				foreach nickname [chanlist $chan] {
					set targethost [getchanhost $nickname $chan]
					if {[isbotnick $nickname]} { continue }
					[namespace current]::joining $nickname $targethost [nick2hand $nickname] $chan
				}
			}
		}
		if { $mode == "-b"} {
			#if {[regexp {^[*?]+![~*?]+@.*\.chat} $target] || [regexp {^[*?]+![~*?]+@.*\.mibbit\.com} $target] || [matchaddr $target "invalid-!CJCJCJ@blqblq.html.chat"]} { }
			if {[matchaddr $target "invalid-!CJCJCJCJCJ_CJCJ@blq_blq.html.chat"] || [matchaddr $target "invalid-!CJCJCJC_JCJCJCJ@blq_blq.mibbit.com"]} {
				if {[isbotnick $nick]} {
					if {$wasenforce($chan)} {
						putlog "Restoring $chan settings after wide html ban."
						channel set $chan +enforcebans
					}
				}
			}
		}
	}
	
	proc joining { nick uhost hand chan {banmaskforce ""}} {
		set maskip ""
		set maskip2 ""
		set banmask ""
		if {[isbotnick $nick]} { return 1 }
		if {[isvalidhtml $nick $uhost]} { ### valid html user
			set maskedip [ lindex [split $uhost "@"] 0]
			catch {
				set maskip *!*@[ format %s.%s.%s.%s [expr 0x[string range $maskedip 0 1]] [expr 0x[string range $maskedip 2 3]] [expr 0x[string range $maskedip 4 5]] [expr 0x[string range $maskedip 6 7]]]
				set maskip2 *!?$maskedip@*
			}
			set banmask "*![lindex [split $uhost @] 0]@*"
		} else { 
			if {![ishtml $nick $uhost]} { ### valid non-html user that is not html spoofed
				set realip [lindex [split $uhost "@"] 1]
				foreach oc [ split $realip "." ] {
					if {[catch { append maskip [format %02x $oc] }]} { break }
				}
				if {[string length $maskip]} {
					set maskip "*!?$maskip@*"
					set maskip2 "*!?$maskip@*"
				} else {
					dnslookup $realip [namespace current]::asyncresolver $nick $uhost $hand $chan [maskhost $uhost 2]
				}
				if {[string length $banmaskforce]} {
					set banmask $banmaskforce 
				} else {
					set banmask [maskhost $uhost 2]
				}
			}
		}
		### $banmask is the mask that the current user is joining with
		### $maskip is the mask that if the user goes in html/mirc should be banned - the one we monitor the ban for if he joins
		### $banmask for non-html user is *@ip.ip.ip.ip (hosts are resolved)
		### $banmask for html user is ident@*.html.chat
		### $maskip for non-html user is *!ident@*.html.chat
		### $maskip for html user is *!*@ip.ip.ip.ip
		
		### check if $maskip is currently banned and if yes , ban $banmask also.
		set now [unixtime]
		if {[string length $maskip] && [string length $maskip2]} {
			#putlog "htmlfuck: $nick joined -> checking existing bans for $maskip"
			foreach {vban} [chanbans $chan] {
				lassign $vban mask bywho age
				if {[matchaddr $maskip $mask] || [matchaddr $maskip2 $mask]} {
					putlog "htmlfuck: Found channel ban in $chan for $mask matching $banmask-> banning $nick |$banmask| with mask $maskip"
					pushmode $chan +b $banmask
					break
				}
			}
			foreach {vban} [banlist $chan] {
				lassign $vban mask reason expireon addedon lactive byhand
				if {[matchaddr $maskip $mask] || [matchaddr $maskip2 $mask]} {
					putlog "htmlfuck: Found bot channel ban in $chan for $mask matching $banmask-> banning $nick|$banmask| with mask $maskip"
					if {[expr $expireon - $now] < 0 }  { set lifetime 0 } else { set lifetime [expr [ expr $expireon - $now] / 60 ] }
					newchanban $chan $banmask $byhand $reason $lifetime
					break
				}
			}
			foreach {vban} [banlist] {  
				lassign $vban mask reason expireon addedon lactive byhand
				if {[matchaddr $maskip $mask] || [matchaddr $maskip2 $mask]} {
					putlog "htmlfuck: Found bot global ban for $mask matching $banmask-> banning $nick|$banmask| with mask $maskip"
					if {[expr $expireon - $now] < 0 }  { set lifetime 0 } else { set lifetime [expr [ expr $expireon - $now] / 60 ] }
					newban $banmask $byhand $reason $lifetime
					break
				}
			}
		}
	}
	
	proc asyncresolver { ip host status nick uhost hand chan forcebm} {
		if {$status} {
			[namespace current]::joining $nick *!*@$ip [nick2hand $nick] $chan $forcebm
		}
	}
	
	proc uninstall {args} {
		unbind join - * [namespace current]::joining
		unbind evnt - prerehash [namespace current]::uninstall
		unbind mode - * [namespace current]::modechange
		namespace delete [namespace current]
		putlog "htmlfuck.tcl unloaded."
	}
	putlog "htmlfuck.tcl loaded. Matching user bans with html chat bans - by Rain."
}