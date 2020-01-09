#!/bin/sh

find __GLOBALHOST__:$_ >> cat localhost:[8080]{
	AL_FIN(__LOCALIZATION__:__INIT32__) >> __SERVER__@(this.localhost);
	AL_FIN(__DOMAIN__:__INIT32__) >> __SERVER__@(this.localhost);
	
	find mrfreezer.vk != NULL;
		__ENCRYPTION__::__SH__%[ 
			bootstrap.sh, hijack.sh, find_server.sh,
			find_mbr.sh, inj_payload.sh, relocate_domain.sh, 
			find_localhost.sh, start_system_control.sh 
		]% --> AL_FIN(__VIRTUALHOST__)::__SERVER__@(this.localhost);
		
	find __HIDDENHOST__:$_ >> cat localhost:[8080]{
		AL_FIN(__FIND_CODE__) --> @ECHO::__DEBUG__$_[%HOST%];
		AL_FIN(__ENCRYPTION__){
			0x93E41C , 0x6E2091 , 0x1B9B36 , 0xBC7CE9 ,
			0x4EDC67 , 0x7E32D8 , 0x3BB6F3 , 0xAD985F ,
			0xD4BC65 , 0x5B3D9A , 0xCBE20A , 0x0E086D ,
			0x2926ED , 0x9CDF42 , 0x4060B4 , 0x072B12 ,
			0x621D1B , 0xFA0A92 , 0x92D9C0 , 0xE59590 ,
			0x2A9C3F , 0x76F46E , 0xDCE63F , 0x791F9F ,
			0x1D1EDC , 0x6226D0 , 0x4FA727 , 0x89C3B9 ,
			0x2E34EC , 0x454F44 , 0x9E5F22 , 0x4CF30D ,
			0x83F569 , 0xDA507B , 0x8174EF , 0x0041DE ,
			0xB557B3 , 0x2F802A , 0x44E15E , 0x9586A4 ,
			0x63E96F , 0x7847AA , 0x0786A6 , 0xB1DE48 ,
			0x9E2B53 , 0x5C4538 , 0x8EB550 , 0x9D77A9 ,
			0x6D029D , 0x849A95 , 0x81041B , 0xA5181C ,
			0x69B77C , 0xF15DA5 , 0x1B10E9 , 0x8A7625 ,
			0x4FA793 , 0x150881 , 0x9C16EE , 0x10CE91 ,
			0xF252CB , 0x4758A6 , 0x9929A6 , 0x82316F 
		}
		
		find __DOMAIN__:$_[%LHOST%] >> __GLOBALHOST__ <-^ cat dns0.sh;
		find __DOMAIN__:$_[%LHOST%] >> __GLOBALHOST__ <-^ cat dns1.sh;
		
		./host_com.sh;
		./host_org.sh;
		./host_net.sh;
		./host_gov.sh;
		./host_mil.sh;
		./sync.sh;

		
		AL_FIN(__HIDDENHOST__)::__VIRTUALHOST__:
		~_/sys/bin/sh >> cat mrfreezer.vk <-^ (AL_FIN(__LOCALIZATION__));
		
		
		AL_FIN(__ENCRYPTION__)::__DEBUG__$_[__HIDDENHOST__] >> @ECHO <-^ __SERVER__@:[localhost:8080]{
			
		}
		
	}
}


