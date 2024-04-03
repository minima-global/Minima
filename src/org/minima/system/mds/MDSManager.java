package org.minima.system.mds;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;

import javax.net.ssl.SSLSocket;

import org.minima.database.MinimaDB;
import org.minima.database.minidapps.MDSDB;
import org.minima.database.minidapps.MiniDAPP;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.mds.handler.MDSCompleteHandler;
import org.minima.system.mds.pending.PendingCommand;
import org.minima.system.mds.polling.PollStack;
import org.minima.system.mds.runnable.MDSJS;
import org.minima.system.mds.runnable.NullCallable;
import org.minima.system.mds.runnable.api.APICallback;
import org.minima.system.mds.runnable.shutter.SandboxContextFactory;
import org.minima.system.mds.sql.MiniDAPPDB;
import org.minima.system.network.rpc.HTTPSServer;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BaseConverter;
import org.minima.utils.JsonDB;
import org.minima.utils.Maths;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.ZipExtractor;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;
import org.mozilla.javascript.ClassShutter;
import org.mozilla.javascript.Context;
import org.mozilla.javascript.ContextFactory;
import org.mozilla.javascript.NativeJSON;
import org.mozilla.javascript.Scriptable;
import org.mozilla.javascript.ScriptableObject;

public class MDSManager extends MessageProcessor {

	public static final String MDS_INIT 					= "MDS_INIT";
	public static final String MDS_SHUTDOWN 				= "MDS_SHUTDOWN";
	public static final String MDS_POLLMESSAGE 				= "MDS_POLLMESSAGE";
	public static final String MDS_MINIDAPPS_RESETALL 		= "MDS_MINIDAPPS_RESETALL";
	public static final String MDS_MINIDAPPS_RESETSESSIONS 	= "MDS_MINIDAPPS_RESETSESSIONS";
	
	public static final String MDS_MINIDAPPS_INSTALLED 		= "MDS_MINIDAPPS_INSTALLED";
	public static final String MDS_MINIDAPPS_UNINSTALLED 	= "MDS_MINIDAPPS_UNINSTALLED";
	
	/**
	 * Timer Message sent every 10 seconds to MDS apps - frontend / backend
	 */
	public static final String MDS_TIMER_10SECONDS		= "MDS_TIMER_10SECONDS";
	public static final String MDS_TIMER_60SECONDS		= "MDS_TIMER_60SECONDS";
	public static final String MDS_TIMER_1HOUR			= "MDS_TIMER_1HOUR";
	
	/**
	 * Message sent to MiniDAPPs when shutdown occurs.
	 */
	public static final String MDS_SHUTDOWN_MSG			= "MDS_SHUTDOWN";
	
	//The Main File and Command server
	HTTPSServer mMDSFileServer;
	HTTPSServer mMDSCommand;
	
	File mMDSRootFile; 
	
	PollStack mPollStack;
	
	/**
	 * The DECIMAL JS class that is loaded into every JS Backend class
	 */
	//private String DECIMALJS="!function(n){\"use strict\";var h,R,e,o,u=9e15,g=1e9,m=\"0123456789abcdef\",t=\"2.3025850929940456840179914546843642076011014886287729760333279009675726096773524802359972050895982983419677840422862486334095254650828067566662873690987816894829072083255546808437998948262331985283935053089653777326288461633662222876982198867465436674744042432743651550489343149393914796194044002221051017141748003688084012647080685567743216228355220114804663715659121373450747856947683463616792101806445070648000277502684916746550586856935673420670581136429224554405758925724208241314695689016758940256776311356919292033376587141660230105703089634572075440370847469940168269282808481184289314848524948644871927809676271275775397027668605952496716674183485704422507197965004714951050492214776567636938662976979522110718264549734772662425709429322582798502585509785265383207606726317164309505995087807523710333101197857547331541421808427543863591778117054309827482385045648019095610299291824318237525357709750539565187697510374970888692180205189339507238539205144634197265287286965110862571492198849978748873771345686209167058\",r=\"3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989380952572010654858632789\",c={precision:20,rounding:4,modulo:1,toExpNeg:-7,toExpPos:21,minE:-u,maxE:u,crypto:!1},N=!0,f=\"[DecimalError] \",w=f+\"Invalid argument: \",s=f+\"Precision limit exceeded\",a=f+\"crypto unavailable\",L=Math.floor,v=Math.pow,l=/^0b([01]+(\\.[01]*)?|\\.[01]+)(p[+-]?\\d+)?$/i,d=/^0x([0-9a-f]+(\\.[0-9a-f]*)?|\\.[0-9a-f]+)(p[+-]?\\d+)?$/i,p=/^0o([0-7]+(\\.[0-7]*)?|\\.[0-7]+)(p[+-]?\\d+)?$/i,b=/^(\\d+(\\.\\d*)?|\\.\\d+)(e[+-]?\\d+)?$/i,T=1e7,U=7,E=t.length-1,x=r.length-1,y={name:\"[object Decimal]\"};function M(n){var e,i,t,r=n.length-1,s=\"\",o=n[0];if(0<r){for(s+=o,e=1;e<r;e++)t=n[e]+\"\",(i=U-t.length)&&(s+=C(i)),s+=t;o=n[e],(i=U-(t=o+\"\").length)&&(s+=C(i))}else if(0===o)return\"0\";for(;o%10==0;)o/=10;return s+o}function q(n,e,i){if(n!==~~n||n<e||i<n)throw Error(w+n)}function O(n,e,i,t){var r,s,o;for(s=n[0];10<=s;s/=10)--e;return--e<0?(e+=U,r=0):(r=Math.ceil((e+1)/U),e%=U),s=v(10,U-e),o=n[r]%s|0,null==t?e<3?(0==e?o=o/100|0:1==e&&(o=o/10|0),i<4&&99999==o||3<i&&49999==o||5e4==o||0==o):(i<4&&o+1==s||3<i&&o+1==s/2)&&(n[r+1]/s/100|0)==v(10,e-2)-1||(o==s/2||0==o)&&0==(n[r+1]/s/100|0):e<4?(0==e?o=o/1e3|0:1==e?o=o/100|0:2==e&&(o=o/10|0),(t||i<4)&&9999==o||!t&&3<i&&4999==o):((t||i<4)&&o+1==s||!t&&3<i&&o+1==s/2)&&(n[r+1]/s/1e3|0)==v(10,e-3)-1}function D(n,e,i){for(var t,r,s=[0],o=0,u=n.length;o<u;){for(r=s.length;r--;)s[r]*=e;for(s[0]+=m.indexOf(n.charAt(o++)),t=0;t<s.length;t++)s[t]>i-1&&(void 0===s[t+1]&&(s[t+1]=0),s[t+1]+=s[t]/i|0,s[t]%=i)}return s.reverse()}y.absoluteValue=y.abs=function(){var n=new this.constructor(this);return n.s<0&&(n.s=1),_(n)},y.ceil=function(){return _(new this.constructor(this),this.e+1,2)},y.comparedTo=y.cmp=function(n){var e,i,t,r,s=this,o=s.d,u=(n=new s.constructor(n)).d,c=s.s,f=n.s;if(!o||!u)return c&&f?c!==f?c:o===u?0:!o^c<0?1:-1:NaN;if(!o[0]||!u[0])return o[0]?c:u[0]?-f:0;if(c!==f)return c;if(s.e!==n.e)return s.e>n.e^c<0?1:-1;for(e=0,i=(t=o.length)<(r=u.length)?t:r;e<i;++e)if(o[e]!==u[e])return o[e]>u[e]^c<0?1:-1;return t===r?0:r<t^c<0?1:-1},y.cosine=y.cos=function(){var n,e,i=this,t=i.constructor;return i.d?i.d[0]?(n=t.precision,e=t.rounding,t.precision=n+Math.max(i.e,i.sd())+U,t.rounding=1,i=function(n,e){var i,t,r=e.d.length;t=r<32?(i=Math.ceil(r/3),(1/J(4,i)).toString()):(i=16,\"2.3283064365386962890625e-10\");n.precision+=i,e=W(n,1,e.times(t),new n(1));for(var s=i;s--;){var o=e.times(e);e=o.times(o).minus(o).times(8).plus(1)}return n.precision-=i,e}(t,z(t,i)),t.precision=n,t.rounding=e,_(2==o||3==o?i.neg():i,n,e,!0)):new t(1):new t(NaN)},y.cubeRoot=y.cbrt=function(){var n,e,i,t,r,s,o,u,c,f,a=this,h=a.constructor;if(!a.isFinite()||a.isZero())return new h(a);for(N=!1,(s=a.s*v(a.s*a,1/3))&&Math.abs(s)!=1/0?t=new h(s.toString()):(i=M(a.d),(s=((n=a.e)-i.length+1)%3)&&(i+=1==s||-2==s?\"0\":\"00\"),s=v(i,1/3),n=L((n+1)/3)-(n%3==(n<0?-1:2)),(t=new h(i=s==1/0?\"5e\"+n:(i=s.toExponential()).slice(0,i.indexOf(\"e\")+1)+n)).s=a.s),o=(n=h.precision)+3;;)if(f=(c=(u=t).times(u).times(u)).plus(a),t=F(f.plus(a).times(u),f.plus(c),o+2,1),M(u.d).slice(0,o)===(i=M(t.d)).slice(0,o)){if(\"9999\"!=(i=i.slice(o-3,o+1))&&(r||\"4999\"!=i)){+i&&(+i.slice(1)||\"5\"!=i.charAt(0))||(_(t,n+1,1),e=!t.times(t).times(t).eq(a));break}if(!r&&(_(u,n+1,0),u.times(u).times(u).eq(a))){t=u;break}o+=4,r=1}return N=!0,_(t,n,h.rounding,e)},y.decimalPlaces=y.dp=function(){var n,e=this.d,i=NaN;if(e){if(i=((n=e.length-1)-L(this.e/U))*U,n=e[n])for(;n%10==0;n/=10)i--;i<0&&(i=0)}return i},y.dividedBy=y.div=function(n){return F(this,new this.constructor(n))},y.dividedToIntegerBy=y.divToInt=function(n){var e=this.constructor;return _(F(this,new e(n),0,1,1),e.precision,e.rounding)},y.equals=y.eq=function(n){return 0===this.cmp(n)},y.floor=function(){return _(new this.constructor(this),this.e+1,3)},y.greaterThan=y.gt=function(n){return 0<this.cmp(n)},y.greaterThanOrEqualTo=y.gte=function(n){var e=this.cmp(n);return 1==e||0===e},y.hyperbolicCosine=y.cosh=function(){var n,e,i,t,r,s=this,o=s.constructor,u=new o(1);if(!s.isFinite())return new o(s.s?1/0:NaN);if(s.isZero())return u;i=o.precision,t=o.rounding,o.precision=i+Math.max(s.e,s.sd())+4,o.rounding=1,e=(r=s.d.length)<32?(1/J(4,n=Math.ceil(r/3))).toString():(n=16,\"2.3283064365386962890625e-10\"),s=W(o,1,s.times(e),new o(1),!0);for(var c,f=n,a=new o(8);f--;)c=s.times(s),s=u.minus(c.times(a.minus(c.times(a))));return _(s,o.precision=i,o.rounding=t,!0)},y.hyperbolicSine=y.sinh=function(){var n,e,i,t,r=this,s=r.constructor;if(!r.isFinite()||r.isZero())return new s(r);if(e=s.precision,i=s.rounding,s.precision=e+Math.max(r.e,r.sd())+4,s.rounding=1,(t=r.d.length)<3)r=W(s,2,r,r,!0);else{n=16<(n=1.4*Math.sqrt(t))?16:0|n,r=W(s,2,r=r.times(1/J(5,n)),r,!0);for(var o,u=new s(5),c=new s(16),f=new s(20);n--;)o=r.times(r),r=r.times(u.plus(o.times(c.times(o).plus(f))))}return _(r,s.precision=e,s.rounding=i,!0)},y.hyperbolicTangent=y.tanh=function(){var n,e,i=this,t=i.constructor;return i.isFinite()?i.isZero()?new t(i):(n=t.precision,e=t.rounding,t.precision=n+7,t.rounding=1,F(i.sinh(),i.cosh(),t.precision=n,t.rounding=e)):new t(i.s)},y.inverseCosine=y.acos=function(){var n,e=this,i=e.constructor,t=e.abs().cmp(1),r=i.precision,s=i.rounding;return-1!==t?0===t?e.isNeg()?P(i,r,s):new i(0):new i(NaN):e.isZero()?P(i,r+4,s).times(.5):(i.precision=r+6,i.rounding=1,e=e.asin(),n=P(i,r+4,s).times(.5),i.precision=r,i.rounding=s,n.minus(e))},y.inverseHyperbolicCosine=y.acosh=function(){var n,e,i=this,t=i.constructor;return i.lte(1)?new t(i.eq(1)?0:NaN):i.isFinite()?(n=t.precision,e=t.rounding,t.precision=n+Math.max(Math.abs(i.e),i.sd())+4,t.rounding=1,N=!1,i=i.times(i).minus(1).sqrt().plus(i),N=!0,t.precision=n,t.rounding=e,i.ln()):new t(i)},y.inverseHyperbolicSine=y.asinh=function(){var n,e,i=this,t=i.constructor;return!i.isFinite()||i.isZero()?new t(i):(n=t.precision,e=t.rounding,t.precision=n+2*Math.max(Math.abs(i.e),i.sd())+6,t.rounding=1,N=!1,i=i.times(i).plus(1).sqrt().plus(i),N=!0,t.precision=n,t.rounding=e,i.ln())},y.inverseHyperbolicTangent=y.atanh=function(){var n,e,i,t,r=this,s=r.constructor;return r.isFinite()?0<=r.e?new s(r.abs().eq(1)?r.s/0:r.isZero()?r:NaN):(n=s.precision,e=s.rounding,t=r.sd(),Math.max(t,n)<2*-r.e-1?_(new s(r),n,e,!0):(s.precision=i=t-r.e,r=F(r.plus(1),new s(1).minus(r),i+n,1),s.precision=n+4,s.rounding=1,r=r.ln(),s.precision=n,s.rounding=e,r.times(.5))):new s(NaN)},y.inverseSine=y.asin=function(){var n,e,i,t,r=this,s=r.constructor;return r.isZero()?new s(r):(e=r.abs().cmp(1),i=s.precision,t=s.rounding,-1!==e?0===e?((n=P(s,i+4,t).times(.5)).s=r.s,n):new s(NaN):(s.precision=i+6,s.rounding=1,r=r.div(new s(1).minus(r.times(r)).sqrt().plus(1)).atan(),s.precision=i,s.rounding=t,r.times(2)))},y.inverseTangent=y.atan=function(){var n,e,i,t,r,s,o,u,c,f=this,a=f.constructor,h=a.precision,l=a.rounding;if(f.isFinite()){if(f.isZero())return new a(f);if(f.abs().eq(1)&&h+4<=x)return(o=P(a,h+4,l).times(.25)).s=f.s,o}else{if(!f.s)return new a(NaN);if(h+4<=x)return(o=P(a,h+4,l).times(.5)).s=f.s,o}for(a.precision=u=h+10,a.rounding=1,n=i=Math.min(28,u/U+2|0);n;--n)f=f.div(f.times(f).plus(1).sqrt().plus(1));for(N=!1,e=Math.ceil(u/U),t=1,c=f.times(f),o=new a(f),r=f;-1!==n;)if(r=r.times(c),s=o.minus(r.div(t+=2)),r=r.times(c),void 0!==(o=s.plus(r.div(t+=2))).d[e])for(n=e;o.d[n]===s.d[n]&&n--;);return i&&(o=o.times(2<<i-1)),N=!0,_(o,a.precision=h,a.rounding=l,!0)},y.isFinite=function(){return!!this.d},y.isInteger=y.isInt=function(){return!!this.d&&L(this.e/U)>this.d.length-2},y.isNaN=function(){return!this.s},y.isNegative=y.isNeg=function(){return this.s<0},y.isPositive=y.isPos=function(){return 0<this.s},y.isZero=function(){return!!this.d&&0===this.d[0]},y.lessThan=y.lt=function(n){return this.cmp(n)<0},y.lessThanOrEqualTo=y.lte=function(n){return this.cmp(n)<1},y.logarithm=y.log=function(n){var e,i,t,r,s,o,u,c,f=this,a=f.constructor,h=a.precision,l=a.rounding;if(null==n)n=new a(10),e=!0;else{if(i=(n=new a(n)).d,n.s<0||!i||!i[0]||n.eq(1))return new a(NaN);e=n.eq(10)}if(i=f.d,f.s<0||!i||!i[0]||f.eq(1))return new a(i&&!i[0]?-1/0:1!=f.s?NaN:i?0:1/0);if(e)if(1<i.length)s=!0;else{for(r=i[0];r%10==0;)r/=10;s=1!==r}if(N=!1,o=V(f,u=h+5),t=e?Z(a,u+10):V(n,u),O((c=F(o,t,u,1)).d,r=h,l))do{if(o=V(f,u+=10),t=e?Z(a,u+10):V(n,u),c=F(o,t,u,1),!s){+M(c.d).slice(r+1,r+15)+1==1e14&&(c=_(c,h+1,0));break}}while(O(c.d,r+=10,l));return N=!0,_(c,h,l)},y.minus=y.sub=function(n){var e,i,t,r,s,o,u,c,f,a,h,l,d=this,p=d.constructor;if(n=new p(n),!d.d||!n.d)return d.s&&n.s?d.d?n.s=-n.s:n=new p(n.d||d.s!==n.s?d:NaN):n=new p(NaN),n;if(d.s!=n.s)return n.s=-n.s,d.plus(n);if(f=d.d,l=n.d,u=p.precision,c=p.rounding,!f[0]||!l[0]){if(l[0])n.s=-n.s;else{if(!f[0])return new p(3===c?-0:0);n=new p(d)}return N?_(n,u,c):n}if(i=L(n.e/U),a=L(d.e/U),f=f.slice(),s=a-i){for(o=(h=s<0)?(e=f,s=-s,l.length):(e=l,i=a,f.length),(t=Math.max(Math.ceil(u/U),o)+2)<s&&(s=t,e.length=1),e.reverse(),t=s;t--;)e.push(0);e.reverse()}else{for((h=(t=f.length)<(o=l.length))&&(o=t),t=0;t<o;t++)if(f[t]!=l[t]){h=f[t]<l[t];break}s=0}for(h&&(e=f,f=l,l=e,n.s=-n.s),o=f.length,t=l.length-o;0<t;--t)f[o++]=0;for(t=l.length;s<t;){if(f[--t]<l[t]){for(r=t;r&&0===f[--r];)f[r]=T-1;--f[r],f[t]+=T}f[t]-=l[t]}for(;0===f[--o];)f.pop();for(;0===f[0];f.shift())--i;return f[0]?(n.d=f,n.e=S(f,i),N?_(n,u,c):n):new p(3===c?-0:0)},y.modulo=y.mod=function(n){var e,i=this,t=i.constructor;return n=new t(n),!i.d||!n.s||n.d&&!n.d[0]?new t(NaN):!n.d||i.d&&!i.d[0]?_(new t(i),t.precision,t.rounding):(N=!1,9==t.modulo?(e=F(i,n.abs(),0,3,1)).s*=n.s:e=F(i,n,0,t.modulo,1),e=e.times(n),N=!0,i.minus(e))},y.naturalExponential=y.exp=function(){return B(this)},y.naturalLogarithm=y.ln=function(){return V(this)},y.negated=y.neg=function(){var n=new this.constructor(this);return n.s=-n.s,_(n)},y.plus=y.add=function(n){var e,i,t,r,s,o,u,c,f,a,h=this,l=h.constructor;if(n=new l(n),!h.d||!n.d)return h.s&&n.s?h.d||(n=new l(n.d||h.s===n.s?h:NaN)):n=new l(NaN),n;if(h.s!=n.s)return n.s=-n.s,h.minus(n);if(f=h.d,a=n.d,u=l.precision,c=l.rounding,!f[0]||!a[0])return a[0]||(n=new l(h)),N?_(n,u,c):n;if(s=L(h.e/U),t=L(n.e/U),f=f.slice(),r=s-t){for((o=(o=r<0?(i=f,r=-r,a.length):(i=a,t=s,f.length))<(s=Math.ceil(u/U))?s+1:o+1)<r&&(r=o,i.length=1),i.reverse();r--;)i.push(0);i.reverse()}for((o=f.length)-(r=a.length)<0&&(r=o,i=a,a=f,f=i),e=0;r;)e=(f[--r]=f[r]+a[r]+e)/T|0,f[r]%=T;for(e&&(f.unshift(e),++t),o=f.length;0==f[--o];)f.pop();return n.d=f,n.e=S(f,t),N?_(n,u,c):n},y.precision=y.sd=function(n){var e;if(void 0!==n&&n!==!!n&&1!==n&&0!==n)throw Error(w+n);return this.d?(e=k(this.d),n&&this.e+1>e&&(e=this.e+1)):e=NaN,e},y.round=function(){var n=this.constructor;return _(new n(this),this.e+1,n.rounding)},y.sine=y.sin=function(){var n,e,i=this,t=i.constructor;return i.isFinite()?i.isZero()?new t(i):(n=t.precision,e=t.rounding,t.precision=n+Math.max(i.e,i.sd())+U,t.rounding=1,i=function(n,e){var i,t=e.d.length;if(t<3)return W(n,2,e,e);i=16<(i=1.4*Math.sqrt(t))?16:0|i,e=e.times(1/J(5,i)),e=W(n,2,e,e);for(var r,s=new n(5),o=new n(16),u=new n(20);i--;)r=e.times(e),e=e.times(s.plus(r.times(o.times(r).minus(u))));return e}(t,z(t,i)),t.precision=n,t.rounding=e,_(2<o?i.neg():i,n,e,!0)):new t(NaN)},y.squareRoot=y.sqrt=function(){var n,e,i,t,r,s,o=this,u=o.d,c=o.e,f=o.s,a=o.constructor;if(1!==f||!u||!u[0])return new a(!f||f<0&&(!u||u[0])?NaN:u?o:1/0);for(N=!1,t=0==(f=Math.sqrt(+o))||f==1/0?(((e=M(u)).length+c)%2==0&&(e+=\"0\"),f=Math.sqrt(e),c=L((c+1)/2)-(c<0||c%2),new a(e=f==1/0?\"1e\"+c:(e=f.toExponential()).slice(0,e.indexOf(\"e\")+1)+c)):new a(f.toString()),i=(c=a.precision)+3;;)if(t=(s=t).plus(F(o,s,i+2,1)).times(.5),M(s.d).slice(0,i)===(e=M(t.d)).slice(0,i)){if(\"9999\"!=(e=e.slice(i-3,i+1))&&(r||\"4999\"!=e)){+e&&(+e.slice(1)||\"5\"!=e.charAt(0))||(_(t,c+1,1),n=!t.times(t).eq(o));break}if(!r&&(_(s,c+1,0),s.times(s).eq(o))){t=s;break}i+=4,r=1}return N=!0,_(t,c,a.rounding,n)},y.tangent=y.tan=function(){var n,e,i=this,t=i.constructor;return i.isFinite()?i.isZero()?new t(i):(n=t.precision,e=t.rounding,t.precision=n+10,t.rounding=1,(i=i.sin()).s=1,i=F(i,new t(1).minus(i.times(i)).sqrt(),n+10,0),t.precision=n,t.rounding=e,_(2==o||4==o?i.neg():i,n,e,!0)):new t(NaN)},y.times=y.mul=function(n){var e,i,t,r,s,o,u,c,f,a=this.constructor,h=this.d,l=(n=new a(n)).d;if(n.s*=this.s,!(h&&h[0]&&l&&l[0]))return new a(!n.s||h&&!h[0]&&!l||l&&!l[0]&&!h?NaN:h&&l?0*n.s:n.s/0);for(i=L(this.e/U)+L(n.e/U),(c=h.length)<(f=l.length)&&(s=h,h=l,l=s,o=c,c=f,f=o),s=[],t=o=c+f;t--;)s.push(0);for(t=f;0<=--t;){for(e=0,r=c+t;t<r;)u=s[r]+l[t]*h[r-t-1]+e,s[r--]=u%T|0,e=u/T|0;s[r]=(s[r]+e)%T|0}for(;!s[--o];)s.pop();return e?++i:s.shift(),n.d=s,n.e=S(s,i),N?_(n,a.precision,a.rounding):n},y.toBinary=function(n,e){return G(this,2,n,e)},y.toDecimalPlaces=y.toDP=function(n,e){var i=this,t=i.constructor;return i=new t(i),void 0===n?i:(q(n,0,g),void 0===e?e=t.rounding:q(e,0,8),_(i,n+i.e+1,e))},y.toExponential=function(n,e){var i,t=this,r=t.constructor;return i=void 0===n?A(t,!0):(q(n,0,g),void 0===e?e=r.rounding:q(e,0,8),A(t=_(new r(t),n+1,e),!0,n+1)),t.isNeg()&&!t.isZero()?\"-\"+i:i},y.toFixed=function(n,e){var i,t,r=this,s=r.constructor;return i=void 0===n?A(r):(q(n,0,g),void 0===e?e=s.rounding:q(e,0,8),A(t=_(new s(r),n+r.e+1,e),!1,n+t.e+1)),r.isNeg()&&!r.isZero()?\"-\"+i:i},y.toFraction=function(n){var e,i,t,r,s,o,u,c,f,a,h,l,d=this,p=d.d,g=d.constructor;if(!p)return new g(d);if(f=i=new g(1),o=(s=(e=new g(t=c=new g(0))).e=k(p)-d.e-1)%U,e.d[0]=v(10,o<0?U+o:o),null==n)n=0<s?e:f;else{if(!(u=new g(n)).isInt()||u.lt(f))throw Error(w+u);n=u.gt(e)?0<s?e:f:u}for(N=!1,u=new g(M(p)),a=g.precision,g.precision=s=p.length*U*2;h=F(u,e,0,1,1),1!=(r=i.plus(h.times(t))).cmp(n);)i=t,t=r,r=f,f=c.plus(h.times(r)),c=r,r=e,e=u.minus(h.times(r)),u=r;return r=F(n.minus(i),t,0,1,1),c=c.plus(r.times(f)),i=i.plus(r.times(t)),c.s=f.s=d.s,l=F(f,t,s,1).minus(d).abs().cmp(F(c,i,s,1).minus(d).abs())<1?[f,t]:[c,i],g.precision=a,N=!0,l},y.toHexadecimal=y.toHex=function(n,e){return G(this,16,n,e)},y.toNearest=function(n,e){var i=this,t=i.constructor;if(i=new t(i),null==n){if(!i.d)return i;n=new t(1),e=t.rounding}else{if(n=new t(n),void 0===e?e=t.rounding:q(e,0,8),!i.d)return n.s?i:n;if(!n.d)return n.s&&(n.s=i.s),n}return n.d[0]?(N=!1,i=F(i,n,0,e,1).times(n),N=!0,_(i)):(n.s=i.s,i=n),i},y.toNumber=function(){return+this},y.toOctal=function(n,e){return G(this,8,n,e)},y.toPower=y.pow=function(n){var e,i,t,r,s,o,u=this,c=u.constructor,f=+(n=new c(n));if(!(u.d&&n.d&&u.d[0]&&n.d[0]))return new c(v(+u,f));if((u=new c(u)).eq(1))return u;if(t=c.precision,s=c.rounding,n.eq(1))return _(u,t,s);if((e=L(n.e/U))>=n.d.length-1&&(i=f<0?-f:f)<=9007199254740991)return r=I(c,u,i,t),n.s<0?new c(1).div(r):_(r,t,s);if((o=u.s)<0){if(e<n.d.length-1)return new c(NaN);if(0==(1&n.d[e])&&(o=1),0==u.e&&1==u.d[0]&&1==u.d.length)return u.s=o,u}return(e=0!=(i=v(+u,f))&&isFinite(i)?new c(i+\"\").e:L(f*(Math.log(\"0.\"+M(u.d))/Math.LN10+u.e+1)))>c.maxE+1||e<c.minE-1?new c(0<e?o/0:0):(N=!1,c.rounding=u.s=1,i=Math.min(12,(e+\"\").length),(r=B(n.times(V(u,t+i)),t)).d&&O((r=_(r,t+5,1)).d,t,s)&&(e=t+10,+M((r=_(B(n.times(V(u,e+i)),e),e+5,1)).d).slice(t+1,t+15)+1==1e14&&(r=_(r,t+1,0))),r.s=o,N=!0,_(r,t,c.rounding=s))},y.toPrecision=function(n,e){var i,t=this,r=t.constructor;return i=void 0===n?A(t,t.e<=r.toExpNeg||t.e>=r.toExpPos):(q(n,1,g),void 0===e?e=r.rounding:q(e,0,8),A(t=_(new r(t),n,e),n<=t.e||t.e<=r.toExpNeg,n)),t.isNeg()&&!t.isZero()?\"-\"+i:i},y.toSignificantDigits=y.toSD=function(n,e){var i=this.constructor;return void 0===n?(n=i.precision,e=i.rounding):(q(n,1,g),void 0===e?e=i.rounding:q(e,0,8)),_(new i(this),n,e)},y.toString=function(){var n=this,e=n.constructor,i=A(n,n.e<=e.toExpNeg||n.e>=e.toExpPos);return n.isNeg()&&!n.isZero()?\"-\"+i:i},y.truncated=y.trunc=function(){return _(new this.constructor(this),this.e+1,1)},y.valueOf=y.toJSON=function(){var n=this,e=n.constructor,i=A(n,n.e<=e.toExpNeg||n.e>=e.toExpPos);return n.isNeg()?\"-\"+i:i};var F=function(){function S(n,e,i){var t,r=0,s=n.length;for(n=n.slice();s--;)t=n[s]*e+r,n[s]=t%i|0,r=t/i|0;return r&&n.unshift(r),n}function Z(n,e,i,t){var r,s;if(i!=t)s=t<i?1:-1;else for(r=s=0;r<i;r++)if(n[r]!=e[r]){s=n[r]>e[r]?1:-1;break}return s}function P(n,e,i,t){for(var r=0;i--;)n[i]-=r,r=n[i]<e[i]?1:0,n[i]=r*t+n[i]-e[i];for(;!n[0]&&1<n.length;)n.shift()}return function(n,e,i,t,r,s){var o,u,c,f,a,h,l,d,p,g,m,w,v,N,b,E,x,y,M,q,O=n.constructor,D=n.s==e.s?1:-1,F=n.d,A=e.d;if(!(F&&F[0]&&A&&A[0]))return new O(n.s&&e.s&&(F?!A||F[0]!=A[0]:A)?F&&0==F[0]||!A?0*D:D/0:NaN);for(u=s?(a=1,n.e-e.e):(s=T,a=U,L(n.e/a)-L(e.e/a)),M=A.length,x=F.length,g=(p=new O(D)).d=[],c=0;A[c]==(F[c]||0);c++);if(A[c]>(F[c]||0)&&u--,null==i?(N=i=O.precision,t=O.rounding):N=r?i+(n.e-e.e)+1:i,N<0)g.push(1),h=!0;else{if(N=N/a+2|0,c=0,1==M){for(A=A[f=0],N++;(c<x||f)&&N--;c++)b=f*s+(F[c]||0),g[c]=b/A|0,f=b%A|0;h=f||c<x}else{for(1<(f=s/(A[0]+1)|0)&&(A=S(A,f,s),F=S(F,f,s),M=A.length,x=F.length),E=M,w=(m=F.slice(0,M)).length;w<M;)m[w++]=0;for((q=A.slice()).unshift(0),y=A[0],A[1]>=s/2&&++y;f=0,(o=Z(A,m,M,w))<0?(v=m[0],M!=w&&(v=v*s+(m[1]||0)),1<(f=v/y|0)?(s<=f&&(f=s-1),1==(o=Z(l=S(A,f,s),m,d=l.length,w=m.length))&&(f--,P(l,M<d?q:A,d,s))):(0==f&&(o=f=1),l=A.slice()),(d=l.length)<w&&l.unshift(0),P(m,l,w,s),-1==o&&(o=Z(A,m,M,w=m.length))<1&&(f++,P(m,M<w?q:A,w,s)),w=m.length):0===o&&(f++,m=[0]),g[c++]=f,o&&m[0]?m[w++]=F[E]||0:(m=[F[E]],w=1),(E++<x||void 0!==m[0])&&N--;);h=void 0!==m[0]}g[0]||g.shift()}if(1==a)p.e=u,R=h;else{for(c=1,f=g[0];10<=f;f/=10)c++;p.e=c+u*a-1,_(p,r?i+p.e+1:i,t,h)}return p}}();function _(n,e,i,t){var r,s,o,u,c,f,a,h,l,d=n.constructor;n:if(null!=e){if(!(h=n.d))return n;for(r=1,u=h[0];10<=u;u/=10)r++;if((s=e-r)<0)s+=U,o=e,c=(a=h[l=0])/v(10,r-o-1)%10|0;else if(l=Math.ceil((s+1)/U),(u=h.length)<=l){if(!t)break n;for(;u++<=l;)h.push(0);a=c=0,o=(s%=U)-U+(r=1)}else{for(a=u=h[l],r=1;10<=u;u/=10)r++;c=(o=(s%=U)-U+r)<0?0:a/v(10,r-o-1)%10|0}if(t=t||e<0||void 0!==h[l+1]||(o<0?a:a%v(10,r-o-1)),f=i<4?(c||t)&&(0==i||i==(n.s<0?3:2)):5<c||5==c&&(4==i||t||6==i&&(0<s?0<o?a/v(10,r-o):0:h[l-1])%10&1||i==(n.s<0?8:7)),e<1||!h[0])return h.length=0,f?(e-=n.e+1,h[0]=v(10,(U-e%U)%U),n.e=-e||0):h[0]=n.e=0,n;if(0==s?(h.length=l,u=1,l--):(h.length=l+1,u=v(10,U-s),h[l]=0<o?(a/v(10,r-o)%v(10,o)|0)*u:0),f)for(;;){if(0==l){for(s=1,o=h[0];10<=o;o/=10)s++;for(o=h[0]+=u,u=1;10<=o;o/=10)u++;s!=u&&(n.e++,h[0]==T&&(h[0]=1));break}if(h[l]+=u,h[l]!=T)break;h[l--]=0,u=1}for(s=h.length;0===h[--s];)h.pop()}return N&&(n.e>d.maxE?(n.d=null,n.e=NaN):n.e<d.minE&&(n.e=0,n.d=[0])),n}function A(n,e,i){if(!n.isFinite())return j(n);var t,r=n.e,s=M(n.d),o=s.length;return e?(i&&0<(t=i-o)?s=s.charAt(0)+\".\"+s.slice(1)+C(t):1<o&&(s=s.charAt(0)+\".\"+s.slice(1)),s=s+(n.e<0?\"e\":\"e+\")+n.e):r<0?(s=\"0.\"+C(-r-1)+s,i&&0<(t=i-o)&&(s+=C(t))):o<=r?(s+=C(r+1-o),i&&0<(t=i-r-1)&&(s=s+\".\"+C(t))):((t=r+1)<o&&(s=s.slice(0,t)+\".\"+s.slice(t)),i&&0<(t=i-o)&&(r+1===o&&(s+=\".\"),s+=C(t))),s}function S(n,e){var i=n[0];for(e*=U;10<=i;i/=10)e++;return e}function Z(n,e,i){if(E<e)throw N=!0,i&&(n.precision=i),Error(s);return _(new n(t),e,1,!0)}function P(n,e,i){if(x<e)throw Error(s);return _(new n(r),e,i,!0)}function k(n){var e=n.length-1,i=e*U+1;if(e=n[e]){for(;e%10==0;e/=10)i--;for(e=n[0];10<=e;e/=10)i++}return i}function C(n){for(var e=\"\";n--;)e+=\"0\";return e}function I(n,e,i,t){var r,s=new n(1),o=Math.ceil(t/U+4);for(N=!1;;){if(i%2&&K((s=s.times(e)).d,o)&&(r=!0),0===(i=L(i/2))){i=s.d.length-1,r&&0===s.d[i]&&++s.d[i];break}K((e=e.times(e)).d,o)}return N=!0,s}function H(n){return 1&n.d[n.d.length-1]}function i(n,e,i){for(var t,r=new n(e[0]),s=0;++s<e.length;){if(!(t=new n(e[s])).s){r=t;break}r[i](t)&&(r=t)}return r}function B(n,e){var i,t,r,s,o,u,c,f=0,a=0,h=0,l=n.constructor,d=l.rounding,p=l.precision;if(!n.d||!n.d[0]||17<n.e)return new l(n.d?n.d[0]?n.s<0?0:1/0:1:n.s?n.s<0?0:n:NaN);for(c=null==e?(N=!1,p):e,u=new l(.03125);-2<n.e;)n=n.times(u),h+=5;for(c+=t=Math.log(v(2,h))/Math.LN10*2+5|0,i=s=o=new l(1),l.precision=c;;){if(s=_(s.times(n),c,1),i=i.times(++a),M((u=o.plus(F(s,i,c,1))).d).slice(0,c)===M(o.d).slice(0,c)){for(r=h;r--;)o=_(o.times(o),c,1);if(null!=e)return l.precision=p,o;if(!(f<3&&O(o.d,c-t,d,f)))return _(o,l.precision=p,d,N=!0);l.precision=c+=10,i=s=u=new l(1),a=0,f++}o=u}}function V(n,e){var i,t,r,s,o,u,c,f,a,h,l,d=1,p=n,g=p.d,m=p.constructor,w=m.rounding,v=m.precision;if(p.s<0||!g||!g[0]||!p.e&&1==g[0]&&1==g.length)return new m(g&&!g[0]?-1/0:1!=p.s?NaN:g?0:p);if(a=null==e?(N=!1,v):e,m.precision=a+=10,t=(i=M(g)).charAt(0),!(Math.abs(s=p.e)<15e14))return f=Z(m,a+2,v).times(s+\"\"),p=V(new m(t+\".\"+i.slice(1)),a-10).plus(f),m.precision=v,null==e?_(p,v,w,N=!0):p;for(;t<7&&1!=t||1==t&&3<i.charAt(1);)t=(i=M((p=p.times(n)).d)).charAt(0),d++;for(s=p.e,1<t?(p=new m(\"0.\"+i),s++):p=new m(t+\".\"+i.slice(1)),c=o=p=F((h=p).minus(1),p.plus(1),a,1),l=_(p.times(p),a,1),r=3;;){if(o=_(o.times(l),a,1),M((f=c.plus(F(o,new m(r),a,1))).d).slice(0,a)===M(c.d).slice(0,a)){if(c=c.times(2),0!==s&&(c=c.plus(Z(m,a+2,v).times(s+\"\"))),c=F(c,new m(d),a,1),null!=e)return m.precision=v,c;if(!O(c.d,a-10,w,u))return _(c,m.precision=v,w,N=!0);m.precision=a+=10,f=o=p=F(h.minus(1),h.plus(1),a,1),l=_(p.times(p),a,1),r=u=1}c=f,r+=2}}function j(n){return String(n.s*n.s/0)}function $(n,e){var i,t,r;for(-1<(i=e.indexOf(\".\"))&&(e=e.replace(\".\",\"\")),0<(t=e.search(/e/i))?(i<0&&(i=t),i+=+e.slice(t+1),e=e.substring(0,t)):i<0&&(i=e.length),t=0;48===e.charCodeAt(t);t++);for(r=e.length;48===e.charCodeAt(r-1);--r);if(e=e.slice(t,r)){if(r-=t,n.e=i=i-t-1,n.d=[],t=(i+1)%U,i<0&&(t+=U),t<r){for(t&&n.d.push(+e.slice(0,t)),r-=U;t<r;)n.d.push(+e.slice(t,t+=U));e=e.slice(t),t=U-e.length}else t-=r;for(;t--;)e+=\"0\";n.d.push(+e),N&&(n.e>n.constructor.maxE?(n.d=null,n.e=NaN):n.e<n.constructor.minE&&(n.e=0,n.d=[0]))}else n.e=0,n.d=[0];return n}function W(n,e,i,t,r){var s,o,u,c,f=n.precision,a=Math.ceil(f/U);for(N=!1,c=i.times(i),u=new n(t);;){if(o=F(u.times(c),new n(e++*e++),f,1),u=r?t.plus(o):t.minus(o),t=F(o.times(c),new n(e++*e++),f,1),void 0!==(o=u.plus(t)).d[a]){for(s=a;o.d[s]===u.d[s]&&s--;);if(-1==s)break}s=u,u=t,t=o,o=s,0}return N=!0,o.d.length=a+1,o}function J(n,e){for(var i=n;--e;)i*=n;return i}function z(n,e){var i,t=e.s<0,r=P(n,n.precision,1),s=r.times(.5);if((e=e.abs()).lte(s))return o=t?4:1,e;if((i=e.divToInt(r)).isZero())o=t?3:2;else{if((e=e.minus(i.times(r))).lte(s))return o=H(i)?t?2:3:t?4:1,e;o=H(i)?t?1:4:t?3:2}return e.minus(r).abs()}function G(n,e,i,t){var r,s,o,u,c,f,a,h,l,d=n.constructor,p=void 0!==i;if(p?(q(i,1,g),void 0===t?t=d.rounding:q(t,0,8)):(i=d.precision,t=d.rounding),n.isFinite()){for(p?(r=2,16==e?i=4*i-3:8==e&&(i=3*i-2)):r=e,0<=(o=(a=A(n)).indexOf(\".\"))&&(a=a.replace(\".\",\"\"),(l=new d(1)).e=a.length-o,l.d=D(A(l),10,r),l.e=l.d.length),s=c=(h=D(a,10,r)).length;0==h[--c];)h.pop();if(h[0]){if(o<0?s--:((n=new d(n)).d=h,n.e=s,h=(n=F(n,l,i,t,0,r)).d,s=n.e,f=R),o=h[i],u=r/2,f=f||void 0!==h[i+1],f=t<4?(void 0!==o||f)&&(0===t||t===(n.s<0?3:2)):u<o||o===u&&(4===t||f||6===t&&1&h[i-1]||t===(n.s<0?8:7)),h.length=i,f)for(;++h[--i]>r-1;)h[i]=0,i||(++s,h.unshift(1));for(c=h.length;!h[c-1];--c);for(o=0,a=\"\";o<c;o++)a+=m.charAt(h[o]);if(p){if(1<c)if(16==e||8==e){for(o=16==e?4:3,--c;c%o;c++)a+=\"0\";for(c=(h=D(a,r,e)).length;!h[c-1];--c);for(o=1,a=\"1.\";o<c;o++)a+=m.charAt(h[o])}else a=a.charAt(0)+\".\"+a.slice(1);a=a+(s<0?\"p\":\"p+\")+s}else if(s<0){for(;++s;)a=\"0\"+a;a=\"0.\"+a}else if(++s>c)for(s-=c;s--;)a+=\"0\";else s<c&&(a=a.slice(0,s)+\".\"+a.slice(s))}else a=p?\"0p+0\":\"0\";a=(16==e?\"0x\":2==e?\"0b\":8==e?\"0o\":\"\")+a}else a=j(n);return n.s<0?\"-\"+a:a}function K(n,e){if(n.length>e)return n.length=e,!0}function Q(n){return new this(n).abs()}function X(n){return new this(n).acos()}function Y(n){return new this(n).acosh()}function nn(n,e){return new this(n).plus(e)}function en(n){return new this(n).asin()}function tn(n){return new this(n).asinh()}function rn(n){return new this(n).atan()}function sn(n){return new this(n).atanh()}function on(n,e){n=new this(n),e=new this(e);var i,t=this.precision,r=this.rounding,s=t+4;return n.s&&e.s?n.d||e.d?!e.d||n.isZero()?(i=e.s<0?P(this,t,r):new this(0)).s=n.s:!n.d||e.isZero()?(i=P(this,s,1).times(.5)).s=n.s:i=e.s<0?(this.precision=s,this.rounding=1,i=this.atan(F(n,e,s,1)),e=P(this,s,1),this.precision=t,this.rounding=r,n.s<0?i.minus(e):i.plus(e)):this.atan(F(n,e,s,1)):(i=P(this,s,1).times(0<e.s?.25:.75)).s=n.s:i=new this(NaN),i}function un(n){return new this(n).cbrt()}function cn(n){return _(n=new this(n),n.e+1,2)}function fn(n){if(!n||\"object\"!=typeof n)throw Error(f+\"Object expected\");var e,i,t,r=!0===n.defaults,s=[\"precision\",1,g,\"rounding\",0,8,\"toExpNeg\",-u,0,\"toExpPos\",0,u,\"maxE\",0,u,\"minE\",-u,0,\"modulo\",0,9];for(e=0;e<s.length;e+=3)if(i=s[e],r&&(this[i]=c[i]),void 0!==(t=n[i])){if(!(L(t)===t&&s[e+1]<=t&&t<=s[e+2]))throw Error(w+i+\": \"+t);this[i]=t}if(i=\"crypto\",r&&(this[i]=c[i]),void 0!==(t=n[i])){if(!0!==t&&!1!==t&&0!==t&&1!==t)throw Error(w+i+\": \"+t);if(t){if(\"undefined\"==typeof crypto||!crypto||!crypto.getRandomValues&&!crypto.randomBytes)throw Error(a);this[i]=!0}else this[i]=!1}return this}function an(n){return new this(n).cos()}function hn(n){return new this(n).cosh()}function ln(n,e){return new this(n).div(e)}function dn(n){return new this(n).exp()}function pn(n){return _(n=new this(n),n.e+1,3)}function gn(){var n,e,i=new this(0);for(N=!1,n=0;n<arguments.length;)if((e=new this(arguments[n++])).d)i.d&&(i=i.plus(e.times(e)));else{if(e.s)return N=!0,new this(1/0);i=e}return N=!0,i.sqrt()}function mn(n){return n instanceof h||n&&\"[object Decimal]\"===n.name||!1}function wn(n){return new this(n).ln()}function vn(n,e){return new this(n).log(e)}function Nn(n){return new this(n).log(2)}function bn(n){return new this(n).log(10)}function En(){return i(this,arguments,\"lt\")}function xn(){return i(this,arguments,\"gt\")}function yn(n,e){return new this(n).mod(e)}function Mn(n,e){return new this(n).mul(e)}function qn(n,e){return new this(n).pow(e)}function On(n){var e,i,t,r,s=0,o=new this(1),u=[];if(void 0===n?n=this.precision:q(n,1,g),t=Math.ceil(n/U),this.crypto)if(crypto.getRandomValues)for(e=crypto.getRandomValues(new Uint32Array(t));s<t;)429e7<=(r=e[s])?e[s]=crypto.getRandomValues(new Uint32Array(1))[0]:u[s++]=r%1e7;else{if(!crypto.randomBytes)throw Error(a);for(e=crypto.randomBytes(t*=4);s<t;)214e7<=(r=e[s]+(e[s+1]<<8)+(e[s+2]<<16)+((127&e[s+3])<<24))?crypto.randomBytes(4).copy(e,s):(u.push(r%1e7),s+=4);s=t/4}else for(;s<t;)u[s++]=1e7*Math.random()|0;for(t=u[--s],n%=U,t&&n&&(r=v(10,U-n),u[s]=(t/r|0)*r);0===u[s];s--)u.pop();if(s<0)u=[i=0];else{for(i=-1;0===u[0];i-=U)u.shift();for(t=1,r=u[0];10<=r;r/=10)t++;t<U&&(i-=U-t)}return o.e=i,o.d=u,o}function Dn(n){return _(n=new this(n),n.e+1,this.rounding)}function Fn(n){return(n=new this(n)).d?n.d[0]?n.s:0*n.s:n.s||NaN}function An(n){return new this(n).sin()}function Sn(n){return new this(n).sinh()}function Zn(n){return new this(n).sqrt()}function Pn(n,e){return new this(n).sub(e)}function Rn(n){return new this(n).tan()}function Ln(n){return new this(n).tanh()}function Tn(n){return _(n=new this(n),n.e+1,1)}(h=function n(e){var i,t,r;function s(n){var e,i,t,r=this;if(!(r instanceof s))return new s(n);if(n instanceof(r.constructor=s))return r.s=n.s,void(N?!n.d||n.e>s.maxE?(r.e=NaN,r.d=null):n.e<s.minE?(r.e=0,r.d=[0]):(r.e=n.e,r.d=n.d.slice()):(r.e=n.e,r.d=n.d?n.d.slice():n.d));if(\"number\"==(t=typeof n)){if(0===n)return r.s=1/n<0?-1:1,r.e=0,void(r.d=[0]);if(r.s=n<0?(n=-n,-1):1,n===~~n&&n<1e7){for(e=0,i=n;10<=i;i/=10)e++;return void(r.d=N?s.maxE<e?(r.e=NaN,null):e<s.minE?[r.e=0]:(r.e=e,[n]):(r.e=e,[n]))}return 0*n!=0?(n||(r.s=NaN),r.e=NaN,void(r.d=null)):$(r,n.toString())}if(\"string\"!==t)throw Error(w+n);return 45===(i=n.charCodeAt(0))?(n=n.slice(1),r.s=-1):(43===i&&(n=n.slice(1)),r.s=1),b.test(n)?$(r,n):function(n,e){var i,t,r,s,o,u,c,f,a;if(\"Infinity\"===e||\"NaN\"===e)return+e||(n.s=NaN),n.e=NaN,n.d=null,n;if(d.test(e))i=16,e=e.toLowerCase();else if(l.test(e))i=2;else{if(!p.test(e))throw Error(w+e);i=8}for(o=0<=(s=(e=0<(s=e.search(/p/i))?(c=+e.slice(s+1),e.substring(2,s)):e.slice(2)).indexOf(\".\")),t=n.constructor,o&&(s=(u=(e=e.replace(\".\",\"\")).length)-s,r=I(t,new t(i),s,2*s)),s=a=(f=D(e,i,T)).length-1;0===f[s];--s)f.pop();return s<0?new t(0*n.s):(n.e=S(f,a),n.d=f,N=!1,o&&(n=F(n,r,4*u)),c&&(n=n.times(Math.abs(c)<54?v(2,c):h.pow(2,c))),N=!0,n)}(r,n)}if(s.prototype=y,s.ROUND_UP=0,s.ROUND_DOWN=1,s.ROUND_CEIL=2,s.ROUND_FLOOR=3,s.ROUND_HALF_UP=4,s.ROUND_HALF_DOWN=5,s.ROUND_HALF_EVEN=6,s.ROUND_HALF_CEIL=7,s.ROUND_HALF_FLOOR=8,s.EUCLID=9,s.config=s.set=fn,s.clone=n,s.isDecimal=mn,s.abs=Q,s.acos=X,s.acosh=Y,s.add=nn,s.asin=en,s.asinh=tn,s.atan=rn,s.atanh=sn,s.atan2=on,s.cbrt=un,s.ceil=cn,s.cos=an,s.cosh=hn,s.div=ln,s.exp=dn,s.floor=pn,s.hypot=gn,s.ln=wn,s.log=vn,s.log10=bn,s.log2=Nn,s.max=En,s.min=xn,s.mod=yn,s.mul=Mn,s.pow=qn,s.random=On,s.round=Dn,s.sign=Fn,s.sin=An,s.sinh=Sn,s.sqrt=Zn,s.sub=Pn,s.tan=Rn,s.tanh=Ln,s.trunc=Tn,void 0===e&&(e={}),e&&!0!==e.defaults)for(r=[\"precision\",\"rounding\",\"toExpNeg\",\"toExpPos\",\"maxE\",\"minE\",\"modulo\",\"crypto\"],i=0;i<r.length;)e.hasOwnProperty(t=r[i++])||(e[t]=this[t]);return s.config(e),s}(c)).default=h.Decimal=h,t=new h(t),r=new h(r),\"function\"==typeof define&&define.amd?define(function(){return h}):\"undefined\"!=typeof module&&module.exports?(\"function\"==typeof Symbol&&\"symbol\"==typeof Symbol.iterator&&(y[Symbol.for(\"nodejs.util.inspect.custom\")]=y.toString,y[Symbol.toStringTag]=\"Decimal\"),module.exports=h):(n||(n=\"undefined\"!=typeof self&&self&&self.self==self?self:window),e=n.Decimal,h.noConflict=function(){return n.Decimal=e,h},n.Decimal=h)}(this);";
	
	/**
	 * The SQL dbs..
	 */
	Hashtable<String, MiniDAPPDB> mSqlDB 	= new Hashtable<>();
	Object mSQLSyncObject 					= new Object();
	
	/**
	 * The KeyPair JSON
	 */
	Hashtable<String, JsonDB> mKeyPairDB 	= new Hashtable<>();
	Object mKeyPairSyncObject 				= new Object();
	
	/**
	 * Valid MiniDAPPs
	 */
	Hashtable<String, String> mSessionID 	= new Hashtable<>();
	
	/**
	 * The BASE MiniDAPP Password for the MiniHUB
	 */
	String mMiniHUBPassword = null;
	
	/**
	 * All the current Contexts
	 */
	HashMap<String, MDSJS> mRunnables = new HashMap();

	/**
	 * Queue of pending messages to be delivered to mRunnables and deferred initialisations thereof
	 */
	BlockingQueue<Runnable> mRunnableQueue = new ArrayBlockingQueue(1000);

	Thread mRunnablesThread = new Thread(() -> {
		while(true) try {
			mRunnableQueue.take().run();
		} catch (InterruptedException e) {
			MinimaLogger.log("MDS runnable queue processing interrupted: " + e.getMessage());
		}
	});
	
	/**
	 * All the Pending Commands
	 */
	ArrayList<PendingCommand> mPending = new ArrayList<>();
	
	/**
	 * The Current Default MinHUB
	 */
	public String DEFAULT_MINIHUB = "0x00";
	
	/**
	 * Has MDS inited
	 */
	boolean mHasStarted 	= false;
	boolean mIsShuttingDown = false;
	
	/**
	 * List of all the API call objects
	 */
	ArrayList<APICallback> mAPICalls = new ArrayList<>();
	
	/**
	 * Main Constructor
	 */
	public MDSManager() {
		super("MDS");
		
		mPollStack = new PollStack();
		
		//What is the root folder
		mMDSRootFile = new File(GeneralParams.DATA_FOLDER,"mds");
		
		//Is MDS even enabled
		if(!GeneralParams.MDS_ENABLED) {
			MinimaLogger.log("MDS disabled");
			return;
		}else {
			MinimaLogger.log("MDS enabled");
		}

		mRunnablesThread.start();
		
		PostMessage(MDS_INIT);
	}
	
	public boolean hasStarted() {
		return mHasStarted;
	}
	
	public boolean isShuttingDown() {
		return mIsShuttingDown;
	}
	
	public void shutdown() {
		//Is it even enabled
		if(!GeneralParams.MDS_ENABLED) {
			stopMessageProcessor();
			return;
		}
		
		//Send a SHUTDOWN message to all the MiniDAPP WEB sites..
		mPollStack.onlyShutDown();
		
		//This is for the JS Runnables
		Main.getInstance().PostNotifyEvent("MDS_SHUTDOWN", new JSONObject());
		
		//Wait 2 seconds for it to be processed..
		try {Thread.sleep(2000);} catch (InterruptedException e) {}
		
		//Now post a shutdown message - added to stack so will wait for POLL messages
		PostMessage(MDS_SHUTDOWN);
		
		//Waiting for shutdown..
		waitToShutDown();
		
		//No longer started
		mHasStarted = false;
	}
	
	public File getRootMDSFolder() {
		return mMDSRootFile;
	}
	
	public File getWebFolder() {
		return new File(mMDSRootFile, "web");
	}
	
	public File getDataFolder() {
		return new File(mMDSRootFile, "data");
	}
	
	public File getMiniDAPPWebFolder(String zUID) {
		return new File(getWebFolder(), zUID);
	}
	
	public File getMiniDAPPDataFolder(String zUID) {
		return new File(getDataFolder(), zUID);
	}
	
	public File getMiniDAPPFileFolder(String zUID) {
		return new File(getMiniDAPPDataFolder(zUID), "file");
	}
	
	public File getMiniDAPPSQLFolder(String zUID) {
		return new File(getMiniDAPPDataFolder(zUID), "sql");
	}
	
	public File getMiniDAPPKeyPairFolder(String zUID) {
		return new File(getMiniDAPPDataFolder(zUID), "keypair");
	}
	
	public String getMiniHUBPasword() {
		return mMiniHUBPassword;
	}
	
	/**
	 * One check at a time
	 * @throws InterruptedException 
	 */
	public synchronized boolean checkMiniHUBPasword(String zPassword) throws InterruptedException {
		
		if(GeneralParams.MDS_PASSWORD.equals("")) {
			boolean valid = mMiniHUBPassword.replace("-", "").equalsIgnoreCase(zPassword.replace("-", "").trim());
			if(!valid) {
				//PAUSE - this prevents fast checking of passwords
				Thread.sleep(1000);
			}
			
			return valid;
		}
		
		boolean valid = mMiniHUBPassword.equals(zPassword.trim());
		if(!valid) {
			Thread.sleep(1000);
		}
		
		return valid;
	}
	
	public MiniDAPP getMiniDAPP(String zMiniDAPPID) {
		return MinimaDB.getDB().getMDSDB().getMiniDAPP(zMiniDAPPID);
	}
	
	public MiniDAPP getMiniDAPPFromName(String zName) {
		ArrayList<MiniDAPP> allmini = MinimaDB.getDB().getMDSDB().getAllMiniDAPPs();
		for(MiniDAPP mini : allmini) {
			if(mini.getName().equalsIgnoreCase(zName)) {
				return mini;
			}
		}
		
		return null;
	}
	
	/**
	 * Return the MINIDAPPID for a given SESSIONID
	 */
	public String convertSessionID(String zSessionID) {
		return mSessionID.get(zSessionID);
	}
	
	/**
	 * Return the SESSIONID for a given MINIDAPPID
	 */
	public String convertMiniDAPPID(String zMiniDAPPID) {
		
		Enumeration<String> keys = mSessionID.keys();
		while(keys.hasMoreElements()) {
			String sessionid 	= keys.nextElement();
			String minidapp 	= mSessionID.get(sessionid);
			if(minidapp.equals(zMiniDAPPID)) {
				return sessionid;
			}
		}
		
		return "";
	}
	
	public String addPendingCommand(MiniDAPP zMiniDAPP, String zCommand) {
		
		//Create a new pending command
		PendingCommand pc = new PendingCommand(zMiniDAPP.toJSON(), zCommand);
		
		//New Pending Command
		mPending.add(pc);
		
		return pc.getUID();
	}
	
	public ArrayList<PendingCommand> getAllPending(){
		return mPending;
	}
	
	public PendingCommand getPendingCommand(String zUID) {
		for(PendingCommand pending : mPending) {
			if(pending.getUID().equals(zUID)) {
				return pending;
			}
		}
		
		return null;
	}
	
	public boolean removePending(String zUID) {
		ArrayList<PendingCommand> newpending = new ArrayList<>();
		boolean found = false;
		for(PendingCommand pending : mPending) {
			if(!pending.getUID().equals(zUID)) {
				newpending.add(pending);
			}else {
				found = true;
			}
		}

		//Switch
		mPending = newpending;
		
		return found;
	}
	
	public void setMDSKeyPair(String zMiniDAPPID, String zKey, String zValue) {
		
		//Synchronise all access
		synchronized (mKeyPairSyncObject) {
			
			//The file
			File jsondbfile = new File(getMiniDAPPKeyPairFolder(zMiniDAPPID),"keypair.db");
			
			//Have we loaded it already..
			JsonDB jsondb = mKeyPairDB.get(zMiniDAPPID);
			
			//Does it exist
			if(jsondb == null) {
				
				//Create
				jsondb = new JsonDB();
				
				//Load it..
				if(jsondbfile.exists()) {
					jsondb.loadDB(jsondbfile);
				}
				
				//And add to our list
				mKeyPairDB.put(zMiniDAPPID, jsondb);
			}
			
			//Now set the Property
			jsondb.setString(zKey, zValue);
			
			//And save it..
			jsondb.saveDB(jsondbfile);
		}
	}
	
	public String getMDSKeyPair(String zMiniDAPPID, String zKey) {
		
		//Synchronise all access
		synchronized (mKeyPairSyncObject) {
			
			//The file
			File jsondbfile = new File(getMiniDAPPKeyPairFolder(zMiniDAPPID),"keypair.db");
			
			//Have we loaded it already..
			JsonDB jsondb = mKeyPairDB.get(zMiniDAPPID);
			
			//Does it exist
			if(jsondb == null) {
				
				//Create
				jsondb = new JsonDB();
				
				//Load it..
				if(jsondbfile.exists()) {
					jsondb.loadDB(jsondbfile);
				}
				
				//And add to our list
				mKeyPairDB.put(zMiniDAPPID, jsondb);
			}
			
			//Now get the Property
			return jsondb.getString(zKey);
		}
	}
	
	public JSONObject runSQL(String zUID, String zSQL) {
		
		//Are we shutting down..
		if(isShuttingDown()) {
			JSONObject err = new JSONObject();
			err.put("sql", zSQL);
			err.put("status", false);
			err.put("err", "MDS Shutting down..");
			return err;
		}
		
		//The MiniDAPPID
		String minidappid = zUID;
		
		//The final DB
		MiniDAPPDB db = null;
		
		//Synchronise access
		synchronized (mSQLSyncObject) {
			
			//Do we have it..
			db = mSqlDB.get(minidappid);
			
			//Does it exists yet
			if(db == null) {
			
				//Create the DB link
				db = new MiniDAPPDB(zUID);
				
				//The location
				File dbfolder3 = getMiniDAPPSQLFolder(minidappid);
				if(!dbfolder3.exists()) {
					dbfolder3.mkdirs();
				}
				
				try {
					
					//Now create the actual sql db
					db.loadDB(new File(dbfolder3,"sqldb"));
				
				} catch (SQLException e) {
					MinimaLogger.log(e);
					
					JSONObject err = new JSONObject();
					err.put("sql", zSQL);
					err.put("status", false);
					err.put("err", e.toString());
					return err;
				}
				
				//Add to the List
				mSqlDB.put(minidappid, db);
			}
		}
		
		//Now run the SQL
		JSONObject res = db.executeSQL(zSQL);
		
		return res;
	}
	
	public void addAPICall(APICallback zAPICallback) {
		mAPICalls.add(zAPICallback);
	}
	
	public APICallback getAPICallback(String zRandID) {
		APICallback foundapicall = null;
		for(APICallback api : mAPICalls) {
			if(api.getRandID().equals(zRandID)) {
				foundapicall = api;
				break;
			}
		}
		
		//Did we find it..
		if(foundapicall != null) {
			mAPICalls.remove(foundapicall);
		}
		
		return foundapicall;
	}
	
	public void shutdownSQL(String zMiniDAPPID){
		//The final DB
		MiniDAPPDB db = mSqlDB.get(zMiniDAPPID);
		
		if(db != null) {
			db.saveDB(true);
		}
		
		mSqlDB.remove(zMiniDAPPID);
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//Is it even enabled
		if(!GeneralParams.MDS_ENABLED) {
			return;
		}
		
		if(zMessage.getMessageType().equals(MDS_INIT)) {
			
			//Create an SSL server
			mMDSFileServer = new HTTPSServer(GeneralParams.MDSFILE_PORT) {
				
				@Override
				public Runnable getSocketHandler(SSLSocket zSocket) {
					return new MDSFileHandler( getWebFolder() , zSocket, MDSManager.this,mPollStack);
				}
			};
			
			//The Complete Server
			mMDSCommand = new HTTPSServer(GeneralParams.MDSCOMMAND_PORT) {
				
				@Override
				public Runnable getSocketHandler(SSLSocket zSocket) {
					return new MDSCompleteHandler(zSocket, MDSManager.this, mPollStack);
				}
			};
			
			//The MDS Password
			if(GeneralParams.MDS_PASSWORD.equals("")) {
				//Create a NEW Main Password..
				MiniData password 	= MiniData.getRandomData(64);
				String b32			= BaseConverter.encode32(password.getBytes());
				
				mMiniHUBPassword	= b32.substring(2,6)+"-"
									 +b32.substring(7,11)+"-"
									 +b32.substring(12,16)+"-"
									 +b32.substring(17,21);
			
			}else {
				//Pre-set..
				mMiniHUBPassword	= GeneralParams.MDS_PASSWORD;
			}
			
			//Is there a Foler of DAPPs to be installed..
			if(!GeneralParams.MDS_INITFOLDER.equals("") && !MinimaDB.getDB().getUserDB().getMDSINIT()) {
				
				//Scan that folder..
				File[] dapps = new File(GeneralParams.MDS_INITFOLDER).listFiles();
				if(dapps!=null) {
					
					//Cycle through..
					for(File dapp : dapps) {
						installMiniDAPP(dapp, GeneralParams.MDS_WRITE);
					}
				}
				
				//Ok we have done it now..
				MinimaDB.getDB().getUserDB().setMDSINIT(true);
				MinimaDB.getDB().saveUserDB();
			}
			
			//Set up the RHINOJS ContextFactory
			//Weird here when Android doesn't clear the class and the static variable persists..
			if(!ContextFactory.hasExplicitGlobal()) {
				ContextFactory.initGlobal(new SandboxContextFactory());
			}else {
				MinimaLogger.log("MDS RHINOJS INIT hasGlobal Allready!.. may need a restart");
			}
			
			//Install the default MiniHUB..
			doDefaultMiniHUB();
			
			//Scan for MiniDApps
			PostMessage(MDS_MINIDAPPS_RESETALL);
		
			//Post another Message
			PostTimerMessage(new TimerMessage(10000, MDS_TIMER_10SECONDS));
			PostTimerMessage(new TimerMessage(60000, MDS_TIMER_60SECONDS));
			PostTimerMessage(new TimerMessage(60000 * 60, MDS_TIMER_1HOUR));
			
		}else if(zMessage.getMessageType().equals(MDS_SHUTDOWN)) {

			//Notify SQL calls not to happen
			mIsShuttingDown = true;
			
			//Shutdown the Runnables
			MinimaLogger.log("Shutdown MDS runnables..");

			mRunnableQueue.clear();
			mRunnablesThread.interrupt();

			mRunnableQueue.add(() -> {
				for (MDSJS mds : mRunnables.values()) {
					try {
						if (mds != null) mds.shutdown();
					} catch (Exception exc) {
						MinimaLogger.log(exc);
					}
				}
			});
			
			//Shut down the servers
			MinimaLogger.log("Shutdown MDS File and Command servers..");
			if(GeneralParams.MDS_ENABLED) {
				mMDSFileServer.shutdown();
				mMDSCommand.shutdown();
			}
			
			//Save all the DBs
			MinimaLogger.log("Shutdown MDS databases..");
			Enumeration<MiniDAPPDB> dbs = mSqlDB.elements();
			while(dbs.hasMoreElements()) {
				dbs.nextElement().saveDB(false);
			}
			
			stopMessageProcessor();
			
		}else if(zMessage.getMessageType().equals(MDS_TIMER_10SECONDS)) {

			//Create a datat object
			JSONObject data = new JSONObject();
			data.put("timemilli", Long.toString(System.currentTimeMillis()));
			
			//Send a POLL message.. 
			Main.getInstance().PostNotifyEvent(MDS_TIMER_10SECONDS, data);
			
			//Post another Message
			PostTimerMessage(new TimerMessage(10000, MDS_TIMER_10SECONDS));
			
		}else if(zMessage.getMessageType().equals(MDS_TIMER_60SECONDS)) {

			//Create a datat object
			JSONObject data = new JSONObject();
			data.put("timemilli", Long.toString(System.currentTimeMillis()));
			
			//Send a POLL message.. 
			Main.getInstance().PostNotifyEvent(MDS_TIMER_60SECONDS, data);
			
			//Post another Message
			PostTimerMessage(new TimerMessage(60000, MDS_TIMER_60SECONDS));
			
		}else if(zMessage.getMessageType().equals(MDS_TIMER_1HOUR)) {

			//Create a datat object
			JSONObject data = new JSONObject();
			data.put("timemilli", Long.toString(System.currentTimeMillis()));
			
			//Send a POLL message.. 
			Main.getInstance().PostNotifyEvent(MDS_TIMER_1HOUR, data);
			
			//Post another Message
			PostTimerMessage(new TimerMessage(60000 * 60, MDS_TIMER_1HOUR));
			
		}else if(zMessage.getMessageType().equals(MDS_POLLMESSAGE)) {

			// Add a message to the POll..
			JSONObject poll = (JSONObject) zMessage.getObject("poll");
			String to 		= zMessage.getString("to");
			
			//Check for shutdown message - sent at the end so all other messages must have been processed
			if(poll.getString("event").equals("MDS_SHUTDOWN")) {
				MinimaLogger.log("JS RUNNABLES received all POLL messages.. SHUTDOWN started..");
			}
			
			boolean sendtoall = true;
			if(poll.getString("event").equals("MDSAPI")) {
				
				//Get the data
				JSONObject dataobj = (JSONObject) poll.get("data");
				
				//Is it  a response..
				if(!(boolean)dataobj.get("request")) {
					
					//RESPONSE messages are not forwarded
					sendtoall = false;
					
					//Send to the API Call..
					APICallback api = getAPICallback(dataobj.getString("id"));
					if(api != null) {
						
						//Construct a reply..
						JSONObject reply = new JSONObject();
						reply.put("status", dataobj.get("status"));
						reply.put("data", dataobj.get("message"));
						
						//Call it..
						Object[] args = { NativeJSON.parse(api.getContext(), 
									api.getScope(),reply.toString(), new NullCallable()) };
						
						//Call the main MDS Function in JS
						api.getFunction().call(api.getContext(), api.getScope(), api.getScope(), args);
						
					}else {
						//MinimaLogger.log("RUNNABLE NOT FOUND API CALL : "+dataobj.toString());
					}
				}
			}

			//Send message to the runnables first..
			if (sendtoall) {
				mRunnableQueue.add(() -> {
					for (String mdsUuid : mRunnables.keySet()) {
						try {

							if (to.equals("*")) {
								//Send to the runnable
								MDSJS mds = mRunnables.get(mdsUuid);
								if (mds != null) mds.callMainCallback(poll);
							} else {

								//Check the MiniDAPPID
								if (mdsUuid.equals(to)) {
									//Send to the runnable
									MDSJS mds = mRunnables.get(mdsUuid);
									if (mds != null) mds.callMainCallback(poll);
								}
							}

						} catch (Exception exc) {
							MinimaLogger.log(exc, false);
						}
					}
				});
			}
			
			//Add then to the Poll Stack - web minidapps
			mPollStack.addMessage(poll,to);
		
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_RESETSESSIONS)) {
			
			//Clear the Old
			mSessionID.clear();
			
			//Reassign..
			ArrayList<MiniDAPP> dapps = MinimaDB.getDB().getMDSDB().getAllMiniDAPPs();
			for(MiniDAPP dapp : dapps) {
				//Use a 128 random value..  
				String sessionid = MiniData.getRandomData(128).to0xString();
				mSessionID.put(sessionid, dapp.getUID());
			}
			
			//Something has changed
			PostMiniDAPPChange();
			
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_RESETALL)) {

			mRunnableQueue.clear();
			mRunnablesThread.interrupt();

			mRunnableQueue.add(() -> {
				//Shut down all the Context Objkects..
				for (MDSJS mds : mRunnables.values()) {
					if (mds != null) mds.shutdown();
				}
			});
			
			//Now clear
			mRunnables.clear();
			mSessionID.clear();

			//Scan through and see what we have..
			ArrayList<MiniDAPP> dapps = MinimaDB.getDB().getMDSDB().getAllMiniDAPPs();
			for(MiniDAPP dapp : dapps) {
				
				//Set it up
				setupMiniDAPP(dapp);
			}
		
			mHasStarted = true;
		
			//Something has changed
			PostMiniDAPPChange();
			
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_INSTALLED)) {
			
			//Get the MiniDAPP
			MiniDAPP dapp = (MiniDAPP) zMessage.getObject("minidapp");
				
			//Install it..
			setupMiniDAPP(dapp);
		
			//Something has changed
			PostMiniDAPPChange();
			
		}else if(zMessage.getMessageType().equals(MDS_MINIDAPPS_UNINSTALLED)) {
			
			//Remove a MiniDAPP
			String uid = zMessage.getString("uid");

			mRunnableQueue.add(() -> {
				MDSJS mds = mRunnables.get(uid);
				if (mds != null) mds.shutdown();
			});
			mRunnables.remove(uid);
			
			//And now remove the sessionid
			mSessionID.remove(convertMiniDAPPID(uid));
			
			//Something has changed
			PostMiniDAPPChange();
		}
	}

	/**
	 * MiniDAPP installed uninstalled or sessions changed
	 */
	public void PostMiniDAPPChange() {
		Main.getInstance().PostNotifyEvent("MDS_MINIDAPPS_CHANGE", new JSONObject());
	}
	
	/**
	 * Initialise a MiniDAPP
	 */
	private void setupMiniDAPP(MiniDAPP zDAPP) {
		
		//Is there a service.js class
		File service = new File(getMiniDAPPWebFolder(zDAPP.getUID()),"service.js");
		if(service.exists()) {
			
			try {
				//Load the file..
				byte[] serv = MiniFile.readCompleteFile(service);
				String code = new String(serv,MiniString.MINIMA_CHARSET);

				
				//Add the main code to the Runnable
				mRunnableQueue.add(
					() -> {
						//Load it into the service runner..
						Context ctx = Context.enter();
						ctx.setOptimizationLevel(-1);
						ctx.setLanguageVersion(Context.VERSION_1_8);

						//Stop JAVA classes from being run..
						try {
							ctx.setClassShutter(new ClassShutter() {
								public boolean visibleToScripts(String className) {

									//ONLY MDSJS can be called form JS
									if(className.startsWith("org.minima.system.mds.runnable")) {
										return true;
									}

									//MinimaLogger.log("RHINOJS JAVA CLASS DENIED ACCESS : "+className);

									return false;
								}
							});
						}catch(SecurityException sec) {
							if(sec.getMessage().equals("Cannot overwrite existing ClassShutter object")) {
								//we already set it..
							}else {
								MinimaLogger.log(sec);
							}
						}

						//Create the Scope
						Scriptable scope = ctx.initStandardObjects();

						//Create an MDSJS object
						MDSJS mdsjs = new MDSJS(this, zDAPP.getUID(), zDAPP.getName(), ctx, scope);
						ScriptableObject.putProperty(scope, "MDS", Context.javaToJS(mdsjs, scope));

						//Add the DECIMAL.js code..
						//ctx.evaluateString(scope, DECIMALJS, "<decimaljs_"+zDAPP.getUID()+">", 1, null);

						ctx.evaluateString(scope, code, "<mds_"+zDAPP.getUID()+">", 1, null);
						//Add to our map
						mRunnables.put(zDAPP.getUID(), mdsjs);
					}
				);

				//Add placeholder to our map
				mRunnables.put(zDAPP.getUID(), null);
			
			}catch(Exception exc) {
				MinimaLogger.log("ERROR starting service "+zDAPP.getName()+" "+exc);
			}
		}
		
		//Now add a unique random SessionID
		String sessionid = MiniData.getRandomData(128).to0xString();
		mSessionID.put(sessionid, zDAPP.getUID());
	}
	
	/**
	 * Install a MiniDAPP file
	 */
	public boolean installMiniDAPP(File zMiniDAPP, String zWriteAccess) {		
	
		if(!zMiniDAPP.isFile()) {
			return false;
		}
		
		if(!zMiniDAPP.exists()) {
			MinimaLogger.log("MiniDAPP @ "+zMiniDAPP.getAbsolutePath()+" does not exist..");
			return false;
		}
		
		//Now start
		try {
			FileInputStream fis = new FileInputStream(zMiniDAPP);
		
			//Where is it going..
			String rand = MiniData.getRandomData(32).to0xString();
			
			//The file where the package is extracted..
			File dest 	= new File(getWebFolder(),rand);
			if(dest.exists()) {
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
			}
			dest.mkdirs();
			
			//Send it to the extractor..
			ZipExtractor.unzip(fis, dest);
			fis.close();
			
			//Is there a conf file..
			File conf = new File(dest,"dapp.conf");
			if(!conf.exists()) {
				
				MinimaLogger.log("MiniDAPP @ "+zMiniDAPP.getAbsolutePath()+" no conf file..");
				
				//Delete the install
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);	
				
				return false;
			}
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
			
			//Now create the JSON..
			JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
			
			//Is this one set to write
			if(!zWriteAccess.equals("") && jsonconf.containsKey("name")) {
				if(jsonconf.getString("name").equals(zWriteAccess)){
					MinimaLogger.log(jsonconf.getString("name","")+" MiniDAPP set to WRITE access");
					jsonconf.put("permission", "write");
				}else {
					//ALWAYS starts with only READ Permission
					jsonconf.put("permission", "read");
				}
			}else {
				//ALWAYS starts with only READ Permission
				jsonconf.put("permission", "read");
			}
			
			//Create the MiniDAPP
			MiniDAPP md = new MiniDAPP(rand, jsonconf);
			
			//Now add to the DB
			MinimaDB.getDB().getMDSDB().insertMiniDAPP(md);
			
			MinimaLogger.log("MiniDAPP @ "+zMiniDAPP.getAbsolutePath()+" installed..");
			
		} catch (Exception e) {
			MinimaLogger.log(e);
			return false;
		}
		
		return true;
	}
	
	/**
	 * The Default MiniHUB is updated every time you start..
	 */
	private void doDefaultMiniHUB() throws Exception {
		
		//The main MDS DB
		MDSDB mdb = MinimaDB.getDB().getMDSDB();
		
		//Do we have a MiniHUB installed..
		DEFAULT_MINIHUB = MinimaDB.getDB().getUserDB().getDefaultMiniHUB();
		
		//And install some default dapps..
		ArrayList<MiniDAPP> allminis = mdb.getAllMiniDAPPs();
				
		//Check for HUB
		checkInstalled("minihub", "minihub/minihub-0.13.2.mds.zip", allminis, true, true);
		
		//Do we Install the Default MiniDAPPs
		if(GeneralParams.DEFAULT_MINIDAPPS) {
		
			//Pending gets write permissions
			checkInstalled("pending", "default/pending-1.2.0.mds.zip", allminis, true);
			
			//Security MiniDAPP - backups / restore
			checkInstalled("security", "default/security-1.0.0.mds.zip", allminis, true);
			
			//Dappstore gets write permissions
			checkInstalled("dapp store", "default/dapp_store-1.0.8.mds.zip", allminis, true);
			
			//The rest are normal
			checkInstalled("block", "default/block-2.1.3.mds.zip", allminis, false);
			checkInstalled("chatter", "default/chatter-1.7.8.mds.zip", allminis, false);
			checkInstalled("docs", "default/docs-1.4.0.mds.zip", allminis, false);
			checkInstalled("filez", "default/filez-1.9.3.mds.zip", allminis, false);
			checkInstalled("future cash", "default/futurecash-2.4.0.mds.zip", allminis, false);
			checkInstalled("health", "default/health-1.1.5.mds.zip", allminis, false);
			checkInstalled("logs", "default/logs-1.0.2.mds.zip", allminis, false);
			checkInstalled("maxcontacts", "default/maxcontacts-1.10.5.mds.zip", allminis, false);
			checkInstalled("maxsolo", "default/maxsolo-2.5.0.mds.zip", allminis, false);
			checkInstalled("miniswap", "default/miniswap-1.0.4.mds.zip", allminis, false);
			checkInstalled("news feed", "default/news-2.0.mds.zip", allminis, false);
			checkInstalled("script ide", "default/scriptide-2.0.2.mds.zip", allminis, false);
			//checkInstalled("shout out", "default/shoutout-1.0.0.mds.zip", allminis, false);
			checkInstalled("sql bench", "default/sqlbench-0.5.mds.zip", allminis, false);
			checkInstalled("terminal", "default/terminal-2.3.1.mds.zip", allminis, false);
			//checkInstalled("vault", "default/vault-0.9.2.mds.zip", allminis, false);
			checkInstalled("vestr", "default/vestr-1.7.2.mds.zip", allminis, false);
			checkInstalled("wallet", "default/wallet-2.38.1.mds.zip", allminis, false);
		}
	}
	
	private String getVersionFromPath(String zPath) {
		
		//Find the numbers..
		int start = zPath.indexOf("-");
		if(start == -1) {
			return "0";
		}
		
		int end = zPath.indexOf(".mds.zip");
		if(end == -1) {
			return "0";
		}
		
		//Chop it..
		return zPath.substring(start+1,end);
	}
	
	private boolean checkInstalled(String zName, String zResource,  ArrayList<MiniDAPP> zAllDapps, boolean zWrite) {
		return checkInstalled(zName, zResource, zAllDapps, zWrite, false);
	}
	
	private boolean checkInstalled(String zName, String zResource,  ArrayList<MiniDAPP> zAllDapps, boolean zWrite, boolean zIsMiniHUB) {		
		
		//The main MDS DB
		MDSDB mdb = MinimaDB.getDB().getMDSDB();
				
		try {
			
			//Is it already installed
			for(MiniDAPP md : zAllDapps) {
				if(md.getName().equalsIgnoreCase(zName)) {
					
					//Check the Version..
					String newversion = getVersionFromPath(zResource);
					String oldversion = md.getVersion();
					
					//Is it newer
					if(Maths.compareVersions(newversion, oldversion)>0) {
						
						//Update this MiniDAPP..
						updateMiniHUB(zResource, md.getUID(), zWrite);
					}
					
					//Check if this is the MiniHUB..
					if(zName.equals("minihub")) {
						
						//Is it correct
						if(!DEFAULT_MINIHUB.equals(md.getUID())) {
							DEFAULT_MINIHUB = md.getUID();
							
							//And set in UserDB..
							MinimaDB.getDB().getUserDB().setDefaultMiniHUB(DEFAULT_MINIHUB);
							MinimaDB.getDB().saveUserDB();
						}
						
						//Always set MiniHUB to WRITE
						MiniDAPP minihubmd = mdb.getMiniDAPP(DEFAULT_MINIHUB);
						minihubmd.setPermission("write");
						mdb.deleteMiniDAPP(DEFAULT_MINIHUB);
						mdb.insertMiniDAPP(minihubmd);
					}
					
					return true;
				}
			}
			
			//Ok - Install it..
			installDefaultMiniDAPP(zResource,zWrite,zIsMiniHUB);
			
		}catch(Exception exc) {
			MinimaLogger.log("[!] Failed install of "+zName+" @ "+zResource);			
		}
		
		return false;
	}
	
	private void installDefaultMiniDAPP(String zResource, boolean zWrite, boolean zIsMiniHUB) {
		
		//The MiniHUB
		String minidapp = zResource;
		File dest 		= null;
		
		try {
			
			//Get the MiniHUB file..
			InputStream is 	= getClass().getClassLoader().getResourceAsStream(minidapp);
			
			//Get all the data..
			byte[] alldata = MiniFile.readAllBytes(is);
			is.close();
			
			//Create an input stream for the file..
			ByteArrayInputStream bais 	= new ByteArrayInputStream(alldata);
			
			//Where is it going..
			String rand = MiniData.getRandomData(32).to0xString();
			
			//The file where the package is extracted..
			dest 	= new File(getWebFolder(), rand);
			if(dest.exists()) {
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
			}
			boolean mk = dest.mkdirs();
		
			//Send it to the extractor..
			ZipExtractor.unzip(bais, dest);
			bais.close();
			
			//Is there a conf file..
			File conf = new File(dest,"dapp.conf");
			if(!conf.exists()) {
				throw new Exception("No dapp.conf file found @ "+conf.getAbsolutePath());
			}
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
			
			//Now create the JSON..
			JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
			
			//ALWAYS starts with only READ Permission
			if(zWrite) {
				jsonconf.put("permission", "write");
			}else {
				jsonconf.put("permission", "read");
			}
			
			//Which version..
			String name		= jsonconf.getString("name");
			String version 	= jsonconf.getString("version");
			MinimaLogger.log("Installing default MiniDAPP.. "+name+" v"+version);
			
			//Create the MiniDAPP
			MiniDAPP md = new MiniDAPP(rand, jsonconf);
			
			//Now add to the DB
			MDSDB db = MinimaDB.getDB().getMDSDB();
			db.insertMiniDAPP(md);
		
			if(zIsMiniHUB) {
				//Create the webpage
				DEFAULT_MINIHUB = rand;
				
				//And set in UserDB..
				MinimaDB.getDB().getUserDB().setDefaultMiniHUB(rand);
				MinimaDB.getDB().saveUserDB();
			}
			
		}catch(Exception exc) {
			
			//Can log this..
			MinimaLogger.log("[!] Failed install of "+zResource);
			MinimaLogger.log(exc);
			
			//Delete the install
			if(dest != null) {
				MiniFile.deleteFileOrFolder(dest.getAbsolutePath(), dest);
			}
		}
	}
	
	private void updateMiniHUB(String zResource, String zMiniDAPPID, boolean zWrite) {
		
		File minidapp = null;
		
		try {
					
			//Get the MiniHUB file..
			InputStream is = getClass().getClassLoader().getResourceAsStream(zResource);
			
			//Get all the data..
			byte[] alldata = MiniFile.readAllBytes(is);
			is.close();
			
			//Create an input stream for the file..
			ByteArrayInputStream bais 	= new ByteArrayInputStream(alldata);
			
			//Now the MiniDAPP ID
			MDSDB db 			= MinimaDB.getDB().getMDSDB();
			MiniDAPP md 		= db.getMiniDAPP(zMiniDAPPID);
			
			//Get the Conf..
			JSONObject miniconf = md.getConfData();
			
			//Delete ONLY the old WEB files
			String mdsroot 	= getRootMDSFolder().getAbsolutePath();
			minidapp 		= new File(getWebFolder(),zMiniDAPPID);
			if(minidapp.exists()) {
				MiniFile.deleteFileOrFolder(mdsroot, minidapp);
			}
			
			//Extract the new files.. make sure exists
			minidapp.mkdirs();
			
			//Send it to the extractor..
			ZipExtractor.unzip(bais, minidapp);
			bais.close();
		
			//Is there a conf file..
			File conf = new File(minidapp,"dapp.conf");
			if(!conf.exists()) {
				
				//Delete the install
				MiniFile.deleteFileOrFolder(mdsroot, minidapp);	
				
				throw new Exception("No dapp.conf file found");
			}
			
			//Load the Conf file.. to get the data
			MiniString data = new MiniString(MiniFile.readCompleteFile(conf)); 	
			
			//Now create the JSON..
			JSONObject jsonconf = (JSONObject) new JSONParser().parse(data.toString());
			
			//Copy the trust
			if(zWrite) {
				jsonconf.put("permission", "write");
			}
			
			//Which version..
			String version = jsonconf.getString("version");
			MinimaLogger.log("Updating default MiniDAPP.. "+jsonconf.getString("name")+" to v"+version);
			
			//Delete the old..
			db.deleteMiniDAPP(zMiniDAPPID);
			
			//The NEW miniDAPP
			MiniDAPP newmd = new MiniDAPP(zMiniDAPPID, jsonconf);
			
			//Now add to the DB
			db.insertMiniDAPP(newmd);
			
		}catch(Exception exc) {
			
			//Can log this..
			MinimaLogger.log("[!] Failed update of "+zResource);
			MinimaLogger.log(exc);

			if(minidapp != null) {
				//Delete the install
				MiniFile.deleteFileOrFolder(minidapp.getAbsolutePath(), minidapp);
			}
		}
	}
	
	public String getDefaultMiniHUB() {
		return DEFAULT_MINIHUB;
	}
}
