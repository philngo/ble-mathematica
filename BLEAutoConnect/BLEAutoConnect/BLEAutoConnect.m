(* Bluetooth Low Energy/Smart/4.0 Connection Package *)

(* Created by Phil Ngo, Wolfram Research Inc., Jul 22, 2013 *)


BeginPackage["BLEAutoConnect`"]
(* Exported symbols added here with SymbolName::usage *) 

$dataSource = "realdata";

LoadBLELibrary::usage = "LoadBLELibrary[] loads the dynamic library functions.";

BLEInit::usage = "BLEInit[] initializes the library";
(* Deprecated: BLEDeinit::usage = "BLEDeinit[] deinitializes the library"; *)

BLEDiscover::usage = "BLEDiscover[scanTime_Real] discovers all devices with range of the Bluetooth Low Energy hardware. Scan Time is in seconds";
BLEConnect::usage = "BLEConnect[deviceName_String] connects to the device with the name supplied - should match the name returned by BLEDiscover";
BLEDisconnectDevice::usage = "BLEDisconnectDevice[] disconnects from the currently connected device";

BLEDeviceData::usage = "BLEDeviceData[deviceName_String] generates a formatted report containing all the data from a device that the package could automatically detect and interpret.";
BLEDeviceRawData::usage = "BLEDeviceRawData[deviceName_String] generates a report containing all the raw data from a device.";
BLEDeviceInformation::usage = "BLEDeviceInformation[deviceName_String] generates a report containing information about a device."

BLEIsLECapableHardware::usage = "BLEIsLECapableHardware[] returns whether or not the device is LE Capable and prints reason if not";
BLEPrintManagerState::usage = "BLEPrintManagerState[] prints the manager state";

(* Deprecated: BLETrackServiceWithUUID::usage = "BLETrackServiceWithUUID[uuidString] adds the uuid to the list of tracked uuids"; *)
(* Deprecated: BLEUntrackAllServices::usage = "BLEUntrackAllServices[] clears the tracked services list"; *)
(* Deprecated: BLETrackCharacteristicWithUUID::usage = "BLETrackCharacteristicWithUUID[uuidString_String,notify:True|False] adds the uuid to the list of tracked uuids"; *)
(* Deprecated: BLEUntrackAllCharacteristics::usage = "BLEUntrackAllCharacteristics[] clears the tracked services list"; *)
(* Deprecated: BLETrackDescriptorWithUUID::usage = "BLETrackDescriptorWithUUID[uuidString] adds the uuid to the list of tracked uuids"; *)
(* Deprecated: BLEUntrackAllDescriptors::usage = "BLEUntrackAllDescriptors[] clears the tracked services list"; *)

BLEcancelPeripheralConnection::usage = "BLEcancelPeripheralConnection[UUID_String]";
BLEconnectPeripheralWithOptions::usage = "BLEconnectPeripheralWithOptions[UUID_String, notifyOnDisconnect:True|False]";
BLEinitWithDelegate::usage = "BLEinitWithDelegate[]";
BLEretrieveConnectedPeripherals::usage = "BLEretrieveConnectedPeripherals[]";
BLEretrievePeripherals::usage = "BLEretrievePeripherals[peripheralUUIDS__String]";
BLEscanForPeripheralsWithServicesWithOptions::usage = "BLEscanForPeripheralsWithServicesWithOptions[peripheralUUIDS__String, allowDuplicates:True|False]";

BLEdiscoverServices::usage = "BLEdiscoverServices[peripheralName_String, serviceUUIDs_List]";
BLEdiscoverIncludedServicesforService::usage = "BLEdiscoverIncludedServicesforService[peripheralName_String, includedServiceUUIDs_List, serviceUUID_String]";
BLEdiscoverCharacteristicsforService::usage = "BLEdiscoverCharacteristicsforService[peripheralName_String, characteristicUUIDs_List, serviceUUID_String]";
BLEdiscoverDescriptorsForCharacteristic::usage = "BLEdiscoverDescriptorsForCharacteristic[peripheralName_String, characteristicUUID_String]";
BLEreadValueForCharacteristic::usage = "BLEreadValueForCharacteristic[peripheralName_String, characteristicUUID_String]";
BLEreadValueForDescriptor::usage = "BLEreadValueForDescriptor[peripheralName_String, descriptorUUID_String]";
BLEwriteValueforCharacteristicWithtype::usage = "BLEwriteValueforCharacteristicWithtype[peripheralName_String, value_List, characteristicUUID_String, type:True|False]";
BLEwriteValueforDescriptor::usage = "BLEwriteValueforDescriptor[peripheralName_String, value_List, descriptorUUID_String]";
BLEsetNotifyValueforCharacteristic::usage = "BLEsetNotifyValueforCharacteristic[peripheralName_String, notifyValue:True|False, characteristicUUID_String]";

BLESetCustomEventHandler::usage = "BLESetCustomEventHandler[customEventHandler_Symbol] sets a custom event handler so that a user can customize responses to Callbacks";

Begin["`Private`"]
(* Implementation of the package *)
$packageFile = $InputFileName;
$libraryFileName = "BluetoothSmartConnector.dylib";
$libraryPath = FileNameJoin[{FileNameTake[$packageFile,{1,-2}],"LibraryResources",$SystemID,$libraryFileName}];
$libraryLoaded = False;
$asyncTask = Null;
$serviceInfo = Null;
$peripheralName = Null;
$peripheralRawData = {};
$peripheralInterpretedData = {};

LoadBLELibrary[]:= 
Module[{},
	If[ $libraryLoaded,
		LibraryUnload[$libraryPath];
	];
	$BLEInit = LibraryFunctionLoad[ $libraryPath, "BLE_Init", {}, Integer];
	$BLEDeinit = LibraryFunctionLoad[ $libraryPath, "BLE_Deinit", {}, "Void"];
	
	$BLEDiscover = LibraryFunctionLoad[ $libraryPath, "BLE_Discover", LinkObject, LinkObject];
	$BLEConnect = LibraryFunctionLoad[ $libraryPath, "BLE_Connect", {"UTF8String"}, "Boolean"];
	$BLEDisconnectDevice = LibraryFunctionLoad[ $libraryPath, "BLE_DisconnectDevice", {}, "Void"];
	
	$BLEIsLECapableHardware = LibraryFunctionLoad[ $libraryPath, "BLE_isLECapableHardware", {}, "Boolean"];
	$BLEPrintManagerState = LibraryFunctionLoad[ $libraryPath, "BLE_printManagerState", {}, "Void"];
	
	$BLETrackServiceWithUUID = LibraryFunctionLoad[ $libraryPath, "BLE_trackServiceWithUUID", {"UTF8String"}, "Void"];
	$BLEUntrackAllServices = LibraryFunctionLoad[ $libraryPath, "BLE_untrackAllServices", {}, "Void"];
	$BLETrackCharacteristicWithUUID = LibraryFunctionLoad[ $libraryPath, "BLE_trackCharacteristicWithUUID", {"UTF8String","Boolean"}, "Void"];
	$BLEUntrackAllCharacteristics = LibraryFunctionLoad[ $libraryPath, "BLE_untrackAllCharacteristics", {}, "Void"];
	$BLETrackDescriptorWithUUID = LibraryFunctionLoad[ $libraryPath, "BLE_trackDescriptorWithUUID", {"UTF8String"}, "Void"];
	$BLEUntrackAllDescriptors = LibraryFunctionLoad[ $libraryPath, "BLE_untrackAllDescriptors", {}, "Void"];
	
	$BLEcancelPeripheralConnection = LibraryFunctionLoad[ $libraryPath, "BLE_cancelPeripheralConnection", {"UTF8String"}, "Void"];
	$BLEconnectPeripheralWithOptions = LibraryFunctionLoad[ $libraryPath, "BLE_connectPeripheralWithOptions", {"UTF8String","Boolean"}, "Void"];
	$BLEinitWithDelegate = LibraryFunctionLoad[ $libraryPath, "BLE_initWithDelegate", {}, "Void"];
	$BLEretrieveConnectedPeripherals = LibraryFunctionLoad[ $libraryPath, "BLE_retrieveConnectedPeripherals", {}, "Void"];
	$BLEretrievePeripherals = LibraryFunctionLoad[ $libraryPath, "BLE_retrievePeripherals", LinkObject, LinkObject];
	$BLEscanForPeripheralsWithServicesWithOptions = LibraryFunctionLoad[ $libraryPath, "BLE_scanForPeripheralsWithServicesWithOptions", LinkObject, LinkObject];

	$BLEdiscoverServices = LibraryFunctionLoad[ $libraryPath, "BLE_discoverServices", LinkObject, LinkObject];
	$BLEdiscoverIncludedServicesforService = LibraryFunctionLoad[ $libraryPath, "BLE_discoverIncludedServicesforService", LinkObject, LinkObject];
	$BLEdiscoverCharacteristicsforService = LibraryFunctionLoad[ $libraryPath, "BLE_discoverCharacteristicsforService", LinkObject, LinkObject];
	$BLEdiscoverDescriptorsForCharacteristic = LibraryFunctionLoad[ $libraryPath, "BLE_discoverDescriptorsForCharacteristic", {"UTF8String","UTF8String"}, "Void"];
	$BLEreadValueForCharacteristic = LibraryFunctionLoad[ $libraryPath, "BLE_readValueForCharacteristic", {"UTF8String","UTF8String"}, "Void"];
	$BLEreadValueForDescriptor = LibraryFunctionLoad[ $libraryPath, "BLE_readValueForDescriptor", {"UTF8String","UTF8String"}, "Void"];
	$BLEwriteValueforCharacteristicWithtype = LibraryFunctionLoad[ $libraryPath, "BLE_writeValueforCharacteristicWithtype", LinkObject, LinkObject];
	$BLEwriteValueforDescriptor = LibraryFunctionLoad[ $libraryPath, "BLE_writeValueforDescriptor", LinkObject, LinkObject];
	$BLEsetNotifyValueforCharacteristic = LibraryFunctionLoad[ $libraryPath, "BLE_setNotifyValueforCharacteristic", {"UTF8String","Boolean","UTF8String"}, "Void"];
	
	$libraryLoaded = True;
	
	BLEInit[];
]


eventHandler[id_, type_, data_]:=
	Module[{peripheralUUID, advertisementData, serviceData,  serviceUUIDs, localName, manufacturerData, 
			txPowerLevel, services, serviceUUID, serviceInfo, uuidStrings, uuidBytes, characteristics, characteristicDescriptors, characteristicsInfo,
			characteristicUUIDs,characteristicProperties, descriptors, descriptorUUIDs,uuidString,value},
		$CustomEventHandler[type,data];
		Switch[type,
			"isLECapableHardware",
				Print["isLECapableHardware"];
				Print["State:" <> "state"/.data[[1]]];,
			"printManagerState",
				Print["printManagerState"];
				Print["State: " <> "state"/.data[[1]]];,
			"centralManager:didDiscoverPeripheral:advertisementData:RSSI",
				Print["centralManager:didDiscoverPeripheral:advertisementData:RSSI"];
				peripheralUUID = "uuid"/.data[[1]];
				advertisementData = "advertisementData"/.data[[2]];
				localName = "CBAdvertisementDataLocalNameKey"/.advertisementData[[1]];
				manufacturerData = "CBAdvertisementDataManufacturerDataKey"/.advertisementData[[2]];
				serviceData = "CBAdvertisementDataServiceDataKey"/.advertisementData[[3]];
				serviceUUIDs = "CBAdvertisementDataServiceUUIDsKey"/.advertisementData[[4]];
				uuidStrings = serviceUUIDs[[;;,1]];
				uuidBytes = serviceUUIDs[[;;,2]];
				txPowerLevel = "CBAdvertisementDataTxPowerLevelKey"/.advertisementData[[5]];
				(*Print["Local Name: " <> localName];
				Print["PeripheralUUID: " <> ToString[peripheralUUID]];
				Print["Manufacturer Data: " <> ToString[manufacturerData]];
				Print["Service Data: " <> ToString[serviceData] ];
				Print["Service UUID strings: " <> ToString[uuidStrings] ];
				Print["Service UUID bytes: " <> ToString[uuidBytes] ];
				Print["TxPowerLevel: " <> ToString[txPowerLevel] ];
				Print["RSSI: " <> ToString["RSSI"/.data[[3]]]];*),
			"centralManager:didRetrievePeripherals",
				Print["centralManager:didRetrievePeripherals"];
				(*Print["Peripheral count: " <> ToString["peripheralcount"/.data[[1]]]];*),
			"centralManager:didConnectPeripheral",
				Print["centralManager:didConnectPeripheral"];
				Print["Connected to " <> ToString["name"/.data[[1]] ]];,
			"centralManager:didDisconnectPeripheral:error",
				Print["centralManager:didDisconnectPeripheral"];
				Print["Disconnected from " <> ToString["name"/.data[[1]] ]];
				If[Length[data] == 2,
					Print["with error " <> ToString["error"/.data[[2]] ]]];,
			"centralManager:didFailToConnectPeripheral:error",
				Print["centralManager:didFailToConnectPeripheral"];
				Print["Failed to connect to " <> ToString["name"/.data[[1]] ]];
				If[Length[data] == 2,
					Print["with error " <> ToString["error"/.data[[2]] ]]];,
			"peripheral:didDiscoverServices:error",
				Print["peripheral:didDiscoverServices"];
				Print["Discovered services for " <> ToString["name"/.data[[1]] ]];
				services = "services"/.data[[2]];
				uuidStrings = services[[;;,1]];
				uuidBytes = services[[;;,2]];
				(*Print["UUID strings: " <> ToString[uuidStrings]];
				Print["UUID bytes: " <> ToString[uuidBytes]];*)
				$serviceInfo = ImportBLEServices@@uuidStrings;
				If[!MatchQ[$peripheralName,Null],
					BLEdiscoverCharacteristicsforService[$peripheralName,{},#]&/@uuidStrings;
				];
				If[Length[data] == 3,
					Print["with error " <> ToString["error"/.data[[3]] ]]];,
			"peripheral:didDiscoverCharacteristicsForServices:error",
				Print["peripheral:didDiscoverCharacteristicsForServices"];
				serviceUUID = ("uuid"/.data[[1]])[[1]];
				Print["Discovered characteristics for service " <> serviceUUID];
				characteristics = "characteristics"/.data[[2]];
				characteristicDescriptors = characteristics[[;;,1]];
				characteristicUUIDs = characteristics[[;;,2]];
				characteristicProperties = characteristics[[;;,3]];
				uuidStrings = characteristicUUIDs[[;;,1]];
				uuidBytes = characteristicUUIDs[[;;,2]];
				$peripheralRawData = Flatten[Append[DeleteCases[$peripheralRawData,#->_&/@uuidStrings],#->{}&/@uuidStrings],1];
				(*Print["characteristic descriptors: " <> ToString[characteristicDescriptors]];
				Print["Characteristic UUID strings: " <> ToString[uuidStrings] ];
				Print["Characteristic UUID bytes: " <> ToString[uuidBytes] ];
				Print["characteristic properties: " <> ToString[characteristicProperties]];
				Print@$peripheralRawData;*)
				serviceInfo = GetService[serviceUUID,$serviceInfo];
				If[!MatchQ[serviceInfo,Null],
					characteristicsInfo = (GetCharacteristic[#,serviceInfo]&/@uuidStrings);
					If["Mandatory" == ("read"/.("characteristicProperties"/.#)),
						BLEreadValueForCharacteristic[$peripheralName, ("characteristicUUID"/.#)];
					]&/@characteristicsInfo;
					If["Mandatory" == ("notify"/.("characteristicProperties"/.#)),
						BLEsetNotifyValueforCharacteristic[$peripheralName,True, ("characteristicUUID"/.#)];
					]&/@characteristicsInfo;,
					Print@"couldnt find characteristic";
				];
				If[Length[data] == 3,
					Print["with error " <> ToString["error"/.data[[3]] ]]];,
			"peripheral:didUpdateNotificationStateForCharacteristic:error",
				Print["peripheral:didUpdateNotificationStateForCharacteristic"];
				(*Print["Updated notification state for characteristic " <> ToString["uuid"/.data[[1]]]];*)
				If[Length[data] == 2,
					Print["with error " <> ToString["error"/.data[[2]] ]]];,
			"peripheral:didUpdateValueForCharacteristic:error",
				Print["peripheral:didUpdateValueForCharacteristic"];
				uuidString = "uuid"/.data[[1]];
				value = ("value"/.data[[2]])/."value"->{};
				$peripheralRawData = $peripheralRawData/.((uuidString->_)->(uuidString->value));
				$peripheralInterpretedData = InterpretAllData[$peripheralRawData,$serviceInfo];
				(*Print["Updated value for characteristic " <> ToString[uuidString]];
				Print["Value: " <> ToString[value]];
				Print@$peripheralRawData;*)
				If[Length[data] == 3,
					Print["with error " <> ToString["error"/.data[[3]] ]]];,
			"peripheral:didDiscoverDescriptorsForCharacteristic:error",
				Print["peripheral:didDiscoverDescriptorsForCharacteristic"];
				characteristics = "uuid"/.data[[1]];
				descriptors = "descriptors"/.data[[2]];
				descriptorUUIDs = descriptors[[;;,1]];
				(*Print["Characteristic UUID strings: " <> ToString[characteristics[[1]]] ];
				Print["Characteristic UUID bytes: " <> ToString[characteristics[[2]]] ];
				Print["Descriptor UUID strings: " <> ToString[descriptorUUIDs[[;;,1]]] ];
				Print["Descriptor UUID bytes: " <> ToString[descriptorUUIDs[[;;,2]]] ];
				Print["Descriptor values: " <> ToString[descriptors[[;;,2]]] ];*)
				If[Length[data] == 3,
					Print["with error " <> ToString["error"/.data[[3]] ]]];,
			"peripheral:didUpdateValueForDescriptor:error",
				Print["peripheral:didUpdateValueForDescriptor"];
				(*Print["Updated value for descriptor: " <> ToString["uuid"/.data[[1]]]];
				Print["Value: " <> ToString["value"/.data[[2]]]];*)
				If[Length[data] == 3,
					Print["with error " <> ToString["error"/.data[[3]] ]]];,
			"debug",
				Print["Debug" <> ToString[data]],
			_,
				Print["Unrecognised Type: " <> type];
		];
		Return@None;
	]

BLEInit[] :=
	Module[{},
		$asynchTask = Internal`CreateAsynchronousTask[$BLEInit, {}, eventHandler];
	]
	
BLEDeinit[] :=
	Module[{},
		$BLEDeinit[];	
	]

BLEDiscover[scanTime_Real:5.] :=
	Module[{},
		Return@$BLEDiscover[scanTime];
	]
	
BLEConnect[deviceName_String] :=
	Module[{},
		$peripheralName = deviceName;
		Return@$BLEConnect[deviceName];	
	]
	
BLEDisconnectDevice[] :=
	Module[{},
		Return@$BLEDisconnectDevice[];	
	]

BLEIsLECapableHardware[] :=
	Module[{},
		Return@$BLEIsLECapableHardware[];	
	]
	
BLEPrintManagerState[] :=
	Module[{},
		Return@$BLEPrintManagerState[];	
	]
	
BLETrackServiceWithUUID[uuidString_String] :=
	Module[{},
		Return@$BLETrackServiceWithUUID[uuidString];	
	]

BLEUntrackAllServices[] :=
	Module[{},
		Return@$BLEUntrackAllServices[];	
	]

BLETrackCharacteristicWithUUID[uuidString_String, notify:True|False] :=
	Module[{},
		Return@$BLETrackCharacteristicWithUUID[uuidString,notify];	
	]

BLEUntrackAllCharacteristics[] :=
	Module[{},
		Return@$BLEUntrackAllCharacteristics[];	
	]
	
BLETrackDescriptorWithUUID[uuidString_String] :=
	Module[{},
		Return@$BLETrackDescriptorWithUUID[uuidString];	
	]

BLEUntrackAllDescriptors[] :=
	Module[{},
		Return@$BLEUntrackAllDescriptors[];	
	]
	
BLESetCustomEventHandler[customEventHandler_Symbol] :=
	Module[{},
		$CustomEventHandler = customEventHandler;
	]
	
BLEcancelPeripheralConnection[UUID_String] :=
	Module[{},
		Return@$BLEcancelPeripheralConnection[UUID];	
	]
	
BLEconnectPeripheralWithOptions[UUID_String, notifyOnDisconnect:True|False] :=
	Module[{},
		Return@$BLEconnectPeripheralWithOptions[UUID,notifyOnDisconnect];	
	]

BLEinitWithDelegate[] :=
	Module[{},
		Return@$BLEinitWithDelegate[];	
	]

BLEretrieveConnectedPeripherals[] :=
	Module[{},
		Return@$BLEretrieveConnectedPeripherals[];	
	]
	
BLEretrievePeripherals[peripheralUUIDS__String] :=
	Module[{},
		Return@$BLEretrievePeripherals[List[peripheralUUIDS]];	
	]
	
BLEscanForPeripheralsWithServicesWithOptions[peripheralUUIDS_List, allowDuplicates:True|False] :=
	Module[{},
		Return@$BLEscanForPeripheralsWithServicesWithOptions[peripheralUUIDS, allowDuplicates];	
	]

BLEdiscoverServices[peripheralName_String, serviceUUIDs_List] :=
	Module[{},
		Return@$BLEdiscoverServices[peripheralName, serviceUUIDs];	
	]
	
BLEdiscoverIncludedServicesforService[peripheralName_String, includedServiceUUIDs_List, serviceUUID_String] :=
	Module[{},
		Return@$BLEdiscoverIncludedServicesforService[peripheralName, includedServiceUUIDs, serviceUUID];	
	]
	
BLEdiscoverCharacteristicsforService[peripheralName_String, characteristicUUIDs_List, serviceUUID_String] :=
	Module[{},
		Return@$BLEdiscoverCharacteristicsforService[peripheralName, characteristicUUIDs, serviceUUID];	
	]
	
BLEdiscoverDescriptorsForCharacteristic[peripheralName_String, characteristicUUID_String] :=
	Module[{},
		Return@$BLEdiscoverDescriptorsForCharacteristic[peripheralName, characteristicUUID];	
	]
	
BLEreadValueForCharacteristic[peripheralName_String, characteristicUUID_String] :=
	Module[{},
		Return@$BLEreadValueForCharacteristic[peripheralName, characteristicUUID];	
	]
	
BLEreadValueForDescriptor[peripheralName_String, descriptorUUID_String] :=
	Module[{},
		Return@$BLEreadValueForDescriptor[peripheralName, descriptorUUID];	
	]
	
BLEwriteValueforCharacteristicWithtype[peripheralName_String, value_List, characteristicUUID_String, type:True|False] :=
	Module[{},
		Return@$BLEwriteValueforCharacteristicWithtype[peripheralName, value, characteristicUUID, type];	
	]
	
BLEwriteValueforDescriptor[peripheralName_String, value_List, descriptorUUID_String] :=
	Module[{},
		Return@$BLEwriteValueforDescriptor[peripheralName, value, descriptorUUID];	
	]
	
BLEsetNotifyValueforCharacteristic[peripheralName_String, notifyValue:True|False, characteristicUUID_String] :=
	Module[{},
		Return@$BLEsetNotifyValueforCharacteristic[peripheralName, notifyValue, characteristicUUID];	
	]


BLEDeviceData[deviceName_String] :=
	Module[{},
		If[ $dataSource==="exampledata",
			Return@("exampledata"/.Get["BLEAutoConnect`BLEDeviceExampleData`"]),
			Return@$peripheralInterpretedData;
		];
	];

BLEDeviceRawData[deviceName_String] :=
	Module[{},
		Return@$peripheralRawData;
	];
	
BLEDeviceInformation[deviceName_String] :=
	Module[{},
		(*Table[("serviceName"/.data[[service]]) <> " \
(UUID:"<>("serviceUUID"/.data[[service]])<>"):\n"<> \
If[MatchQ["serviceAbstract"/.data[[service]],Null],"",(\
"serviceAbstract"/.data[[service]])<>" \
"]<>If[MatchQ["serviceSummary"/.data[[service]],Null],"",(\
"serviceSummary"/.data[[service]])<>"\n\
"]<>("serviceName"/.data[[service]]) <>" has the following \
characteristic(s):\n" <>StringJoin["-"<>("characteristicName"/.#)<>" \
(UUID: "<>("characteristicUUID"/.#) <>") \
("<>("characteristicRequirement"/.#)<>")\n\
"&/@("serviceCharacteristics"/.data[[service]])],{service,1,Length[\
data]}][[Length[data]]]
data[[Length[data]]];*)
		Return@$serviceData;
	];


(* Helper methods *)

Merge[elements_List, limit_] :=
	Module[{},
		If[Fold[#1 && MatchQ[#2, List] &, limit > 0, Head /@ elements], 
			Return@MapThread[Merge[{##}, limit - 1] &, elements],
			Return@elements
		];
   ];
   
GetService[serviceUUID_String, services_List] :=
	Module[{serviceList},
		serviceList = serviceUUID /. ((("serviceUUID" /. #) -> #)&/@services);
   		Return@
   		If[MatchQ[Head@serviceList, String],
   			Null,
   			serviceList
   		];
   	];

GetCharacteristic[characteristicUUID_String, service_List] :=
	Module[{characteristics, characteristicList},
		characteristics = "serviceCharacteristics" /. service;
		characteristicList = characteristicUUID /. ((("characteristicUUID" /. #) -> #)&/@characteristics);
		Return@
    	If[MatchQ[Head@characteristicList, String],
			Null, 
     		characteristicList
     	];
   	];

InterpretAllData[rawData_List,serviceInfo_List] :=
	Module[{characteristicsInfo,interpretedData},
		characteristicsInfo =
			Flatten[(MapThread[GetCharacteristic[#1,#2]&,
				{(("characteristicUUID"/.#&)/@("serviceCharacteristics"/.#)),
					Table[#,{Length["serviceCharacteristics"/.#]}]}
			]&/@serviceInfo),1];
		interpretedData = 
			Map[("characteristicName"/.#)->InterpretData[(("characteristicUUID"/.#)/.rawData),("characteristicFields"/.#)]&,characteristicsInfo];
		Return@interpretedData;
	]; 

InterpretData[data_,fields_List] :=
	Module[{fieldFormatsToBits,bitsToTake,binaryData,binaryConversionFunctions,
			conversionFunctions,fieldFormats,fieldNames,fieldRequirements,
			fieldBinaryData,fieldData,bitAttributes,i,newData,requiredFields,
			fieldEnumeration},
		If[!ListQ[data],
			Return@Null;
		];
		fieldFormatsToBits = {
			"rfu"->Infinity,
			"gattuuid"->Infinity,
			"boolean"->1,
			"2bit"->2,
			"nibble"->4,
			"8bit"->8,
			"16bit"->16,
			"32bit"->32,
			"uint8"->8,
			"uint12"->12,
			"uint16"->16,
			"uint24"->24,
			"uint32"->32,
			"uint40"->40,
			"uint48"->48,
			"uint64"->64,
			"uint128"->128,
			"sint8"->8,
			"sint12"->12,
			"sint16"->16,
			"sint24"->24,
			"sint32"->32,
			"sint40"->40,
			"sint48"->48,
			"sint64"->64,
			"sint128"->128,
			"float32"->32,
			"float64"->64,
			"SFLOAT"->16,
			"FLOAT"->32,
			"dunit16"->16,
			"utf8s"->Infinity,
			"utf16s"->Infinity,
			"reg-cert-data-list"->Infinity,
			"variable"->Infinity,
			"Array[]" ->Infinity
			};
		binaryConversionFunctions = {
			"rfu"->(FromDigits[#, 2] & /@ Partition[#, 8] &),
			"gattuuid"->(FromDigits[#, 2] & /@ Partition[#, 8] &),
			"boolean"->(("bits"->#)&),
			"2bit"->(("bits"->#)&),
			"nibble"->(("bits"->#)&),
			"8bit"->(("bits"->#)&),
			"16bit"->(("bits"->#)&),
			"32bit"->(("bits"->#)&),
			"uint8"->(FromDigits[#,2]&),
			"uint12"->(FromDigits[#,2]&),
			"uint16"->(FromDigits[#,2]&),
			"uint24"->(FromDigits[#,2]&),
			"uint32"->(FromDigits[#,2]&),
			"uint40"->(FromDigits[#,2]&),
			"uint48"->(FromDigits[#,2]&),
			"uint64"->(FromDigits[#,2]&),
			"uint128"->(FromDigits[#,2]&),
			"sint8"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"sint12"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"sint16"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"sint24"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"sint32"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"sint40"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"sint48"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"sint64"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"sint128"->(If[Take[#, 1][[1]] == 1, -1, 1]*FromDigits[Drop[#,1],2]&),
			"float32"->(If[Take[#, 1][[1]] == 1, -1, 1]*2^(FromDigits[Take[#, {2, 9}], 2] - 127)*(1 + FromDigits[Drop[#, 9], 2]/(2^23)) &),
			"float64"->(If[Take[#, 1][[1]] == 1, -1, 1]*2^(FromDigits[Take[#, {2, 12}], 2] - 1023)*(1 + FromDigits[Drop[#, 12], 2]/(2^52)) &),
			"SFLOAT"->(If[Take[#, 1][[1]] == 1, -1, 1]*2^(FromDigits[Take[#, {2, 6}], 2] - 127)*(1 + FromDigits[Drop[#, 6], 2]/(2^10)) &),
			"FLOAT"->(If[Take[#, 1][[1]] == 1, -1, 1]*2^(FromDigits[Take[#, {2, 9}], 2] - 127)*(1 + FromDigits[Drop[#, 9], 2]/(2^23)) &),
			"dunit16"->(FromDigits[#,2]&),
			"utf8s"->(FromCharacterCode[FromDigits[#, 2] & /@ Partition[#, 8]]&),
			"utf16s"->(FromCharacterCode[FromDigits[#, 2] & /@ Partition[#, 16]]&),
			"reg-cert-data-list"->(FromDigits[#, 2] & /@ Partition[#, 8] &),
			"variable"->(FromDigits[#, 2] & /@ Partition[#, 8] &),
			"Array[]" ->(FromDigits[#, 2] & /@ Partition[#, 8] &)
			};
		fieldFormats =  ("fieldFormat"/.#)&/@fields;
		fieldNames =  ("fieldName"/.#)&/@fields;
		fieldRequirements =  ("fieldRequirement"/.#)&/@fields;
		bitsToTake = fieldFormats/.fieldFormatsToBits;
		conversionFunctions = fieldFormats/.binaryConversionFunctions;
		binaryData = Flatten[IntegerDigits[#, 2, 8] & /@data];
		fieldBinaryData = {};
		fieldData = {};
		requiredFields = {"Mandatory"};
		For[i = 1, i <= Length[fields], i += 1,
			newData = Null;
			If[MemberQ[requiredFields,fieldRequirements[[i]]],
				If[!MatchQ[binaryData,{}],
					If[MatchQ[bitsToTake[[i]],Infinity],
						newData = conversionFunctions[[i]]@binaryData,
						newData = conversionFunctions[[i]]@Take[binaryData,bitsToTake[[i]]];
						binaryData = Drop[binaryData,bitsToTake[[i]]];
					];
				];
				If[MatchQ[newData,"bits"->_],
					bitAttributes = "bitAttributes"/.fields[[i]];
					If[!MatchQ[bitAttributes,Null],
						If[fieldNames[[i]] == "Flags" ||fieldNames[[i]] == "Flag",
							requiredFields = ExtractFieldRequirements["bits"/.newData,bitAttributes];
						];
						newData = InterpretBits["bits"/.newData,bitAttributes];,
						fieldEnumeration = "fieldEnumeration"/.fields[[i]];
						If[!MatchQ[fieldEnumeration,Null],
							newData = InterpretEnumeration["bits"/.newData,fieldEnumeration];,
							newData = List@@("bits"/.newData);
						];
					];
				];
				fieldData = Append[fieldData,fieldNames[[i]]->newData];
			];
		];
		Return@fieldData;
	];
	
InterpretBits[bitData_List,bitAttributes_List] :=
	Module[{bitAttribute,bitSize,bitIndex,attributeIndex,parsedData,key,data},
		parsedData = {};
		bitIndex = 1;
		attributeIndex = 1;
		For[attributeIndex = 1, attributeIndex <= Length[bitAttributes],attributeIndex += 1,
			bitAttribute = bitAttributes[[attributeIndex]];
			bitSize = ToExpression@("bitSize"/.bitAttribute);
			key = ToString[FromDigits[Take[bitData,{bitIndex, bitIndex + bitSize-1}],2]];
			bitIndex += bitSize;
			data = key/.("bitEnumeration"/.bitAttribute);
			parsedData = Append[parsedData,("bitName"/.bitAttribute)->data];
		];
		Return@parsedData;
	];

InterpretEnumeration[bitData_List,enumeration_List] :=
	Module[{},
		Return[(ToString[FromDigits[bitData,2]]/.enumeration)/.({value_,description_}->{"value"->value,"description"->description})];
	];

ExtractFieldRequirements[bitData_List,bitAttributes_List] :=
	Module[{bitAttribute,bitSize,bitIndex,attributeIndex,key,fieldRequirements,data},
		fieldRequirements = {"Mandatory"};
		bitIndex = 1;
		attributeIndex = 1;
		For[attributeIndex = 1, attributeIndex <= Length[bitAttributes],attributeIndex += 1,
			bitAttribute = bitAttributes[[attributeIndex]];
			bitSize = ToExpression@("bitSize"/.bitAttribute);
			key = ToString[FromDigits[Take[bitData,{bitIndex, bitIndex + bitSize-1}],2]];
			bitIndex += bitSize;
			data = key/.("bitRequires"/.bitAttribute);
			If[!MatchQ[data,Null],
				fieldRequirements = Append[fieldRequirements,data];
			];
		];
		Return@fieldRequirements;
	];

ImportBLEServices[UUIDs__] :=
	Module[{specificationTypes, serviceSpecifications, 
			serviceInformativeText, serviceAbstracts , serviceSummaries, 
			allCharacteristicSpecifications, services, serviceNames, 
			serviceTypes, serviceUUIDs, serviceCharacteristics, 
			characteristicRequirements, characteristicPropertiesRaw, 
			propertyRead, propertyWrite, propertyWriteWithoutResponse, 
			propertySignedWrite, propertyReliableWrite, propertyNotify, 
			propertyIndicate, propertyWritableAuxiliaries, propertyBroadcast, 
			characteristicProperties, characteristicAttributes, serviceInfo, 
			characteristicTypes, characteristics, characteristicNames,
    		characteristicUUIDs, characteristicFields, fieldNames,
			fieldFormats, fieldRequirements, fieldUnits, fieldDecimalExponent,
			fieldInformativeText, fieldEnumeration, fieldBitFieldRaw, bitFieldBit,
			bitFieldBitSize, bitFieldBitName, bitFieldBitEnumeration,
			bitFieldBitRequires,bitFieldBitAttributes, fieldAttributes},
		specificationTypes = {
			"1800" -> "generic_access",
			"1801" -> "generic_attribute",
			"1802" -> "immediate_alert",
			"1803" -> "link_loss",
			"1804" -> "tx_power",
			"1805" -> "current_time",
			"1806" -> "reference_time_update",
			"1807" -> "next_dst_change",
			"1808" -> "glucose",
			"1809" -> "health_thermometer",
			"180A" -> "device_information",
			"180D" -> "heart_rate",
			"180E" -> "phone_alert_status",
			"180F" -> "battery_service",
			"1810" -> "blood_pressure",
			"1811" -> "alert_notification",
	     	"1812" -> "human_interface_device",
	     	"1813" -> "scan_parameters",
	     	"1814" -> "running_speed_and_cadence",
	     	"1816" -> "cycling_speed_and_cadence",
	     	"1818" -> "cycling_power",
	     	"1819" -> "location_and_navigation"};
		serviceSpecifications = 
		
		Import[FileNameJoin[{"BLEAutoConnect", "GATTSpecifications",
			"services", "org.bluetooth.service." <> # <> ".xml"}]] & \
				/@ Union @@ Map[Cases[{UUIDs}, #] &, specificationTypes];
		services = Cases[serviceSpecifications, XMLElement["Service", _, _], Infinity];
	   	serviceNames = Map["serviceName" -> # &, "name" /. (Part[#, 2] & /@ services)];
	   	serviceTypes = Map["serviceType" -> # &, "type" /. (Part[#, 2] & /@ services)];
	   	serviceUUIDs = Map["serviceUUID" -> # &, "uuid" /. (Part[#, 2] & /@ services)];
	   	serviceInformativeText = Cases[#, XMLElement["InformativeText", _, _], Infinity] & /@ services;
	   	serviceAbstracts = "serviceAbstract" -> 
			If[Cases[#, XMLElement["Abstract", _, _], Infinity] != {}, 
				Cases[#, XMLElement["Abstract", _, _], Infinity][[1, 3, 1]], 
	        	Null
	        ] & /@ serviceInformativeText;
		serviceSummaries = "serviceSummary" -> 
	       	If[Cases[#, XMLElement["Summary", _, _], Infinity] != {}, 
	        	Cases[#, XMLElement["Summary", _, _], Infinity][[1, 3, 1]], 
	        	Null
	       	] & /@ serviceInformativeText;
	   	serviceCharacteristics = Cases[#, XMLElement["Characteristic", _, _], Infinity] & /@ services;
	   	characteristicTypes = "type" /. Map[Part[#, 2] &, serviceCharacteristics, {2}];
	   	characteristicRequirements = 
	   		Map["characteristicRequirement" -> Part[#, 3, 1] &, 
	    		Map[Cases[#[[3]], XMLElement["Requirement", _, _], Infinity][[1]] &,
					serviceCharacteristics,
				{2}],
			{2}];
		characteristicPropertiesRaw = 
	    	Map[Cases[#[[3]], XMLElement["Properties", _, _], Infinity][[1]] &,
	    		serviceCharacteristics,
			{2}];
		propertyRead =
			Map["read" -> Cases[#, XMLElement["Read", _, _], Infinity][[1, 3, 1]] &, 
	     		characteristicPropertiesRaw,
			{2}];
	   	propertyWrite =
	    	Map["write" -> Cases[#, XMLElement["Write", _, _], Infinity][[1, 3, 1]] &, 
	     		characteristicPropertiesRaw,
	     	{2}];
	   	propertyWriteWithoutResponse =
	    	Map["writeWithoutResponse" ->  Cases[#, XMLElement["WriteWithoutResponse", _, _], Infinity][[1, 3, 1]] &,
	        	characteristicPropertiesRaw,
			{2}];
		propertySignedWrite =
			Map["signedWrite" -> Cases[#, XMLElement["SignedWrite", _, _], Infinity][[1, 3, 1]] &,
				characteristicPropertiesRaw,
			{2}];
	   	propertyReliableWrite =
			Map["reliableWrite" -> Cases[#, XMLElement["ReliableWrite", _, _], Infinity][[1, 3, 1]] &,
				characteristicPropertiesRaw,
			{2}];
		propertyNotify =
			Map["notify" -> Cases[#, XMLElement["Notify", _, _], Infinity][[1, 3, 1]] &, 
				characteristicPropertiesRaw,
			{2}];
		propertyIndicate =
	    	Map["indicate" -> Cases[#, XMLElement["Indicate", _, _], Infinity][[1, 3, 1]] &, 
	    		characteristicPropertiesRaw,
	    	{2}];
	   	propertyWritableAuxiliaries =
	    	Map["writableAuxiliaries" -> Cases[#, XMLElement["WritableAuxiliaries", _, _], Infinity][[1, 3, 1]] &,
				characteristicPropertiesRaw,
			{2}];
	   	propertyBroadcast =
			Map["broadcast" -> Cases[#, XMLElement["Broadcast", _, _], Infinity][[1, 3, 1]] &,
	      		characteristicPropertiesRaw,
	      	{2}];
	   	characteristicProperties = 
	    	Map[ "characteristicProperties" -> # &, 
	     		Merge[{ propertyRead, propertyWrite, propertyWriteWithoutResponse,
	        			propertySignedWrite, propertyReliableWrite, propertyNotify, 
	      				propertyIndicate, propertyWritableAuxiliaries, propertyBroadcast},
	      		Infinity],
	      	{2}];
		serviceInfo = 
			Thread[List[serviceNames, serviceTypes, serviceUUIDs, 
						serviceAbstracts, serviceSummaries]];
	   	allCharacteristicSpecifications = 
	    	Map[Import[FileNameJoin[{"BLEAutoConnect", "GATTSpecifications",
			"characteristics", # <> ".xml"}]] &,
				characteristicTypes,
			{2}];
	   	characteristics = 
			Map[Cases[#, XMLElement["Characteristic", _, _], Infinity][[1]] &,
	      		allCharacteristicSpecifications,
			{2}];
	   	characteristicNames = 
			Map["characteristicName" -> "name" /. Part[#, 2] &, 
				characteristics,
			{2}];
	   	characteristicTypes = 
			Map["characteristicType" -> "type" /. Part[#, 2] &, 
				characteristics,
			{2}];
		characteristicUUIDs = 
			Map["characteristicUUID" -> "uuid" /. Part[#, 2] &, 
				characteristics,
			{2}];
	   	characteristicFields = 
			Map[Cases[#[[3]], XMLElement["Field", _, _], Infinity] &, 
	     		characteristics,
			{2}];
		fieldNames = 
	    	Map["fieldName" -> "name" /. #[[2]] &, 
				characteristicFields,
			{3}];
		fieldRequirements = 
			Map["fieldRequirement" -> If[#1 == {}, Null, #1[[1, 3, 1]]] &, 
				Map[Cases[#[[3]], XMLElement["Requirement", _, _], Infinity] &, 
					characteristicFields,
				{3}],
			{3}];
		fieldFormats = 
			Map["fieldFormat" -> If[#1 == {}, Null, #1[[1, 3, 1]]] &, 
				Map[Cases[#[[3]], XMLElement["Format", _, _], Infinity] &, 
	      			characteristicFields,
	      		{3}],
	      	{3}];
		fieldUnits = 
			Map["fieldUnit" -> If[#1 == {}, Null, #1[[1, 3, 1]]] &, 
				Map[Cases[#[[3]], XMLElement["Unit", _, _], Infinity] &, 
					characteristicFields,
				{3}],
			{3}];
		fieldDecimalExponent = 
			Map["fieldDecimalExponent" -> If[#1 == {}, Null, #1[[1, 3, 1]]] &,
				Map[Cases[#[[3]], XMLElement["DecimalExponent", _, _], Infinity] &,
					characteristicFields,
				{3}],
			{3}];
	   	fieldInformativeText = 
			Map["fieldInformativeText" -> If[#1 == {}, Null, #1[[1, 3, 1]]] &,
				Map[Cases[#[[3]], XMLElement["InformativeText", _,Except[{},_]], Infinity] &,
					characteristicFields,
				{3}],
			{3}];
		fieldEnumeration = 
			Map["fieldEnumeration" ->
				If[#1 == {} || MatchQ[#1, Null],
					Null, 
	        		("key" -> {"value", "description"} /.
						Cases[#1, XMLElement["Enumeration", _, _], Infinity][[All, 2]])/.
						{("key" -> {"value", "description"}) -> Null,
						(key_ -> {value_, "description"})->(key->{value,Null})}
				] &, 
				Map[Cases[#[[3]], XMLElement["Enumerations", _, _], 2] &, 
					characteristicFields,
				{3}],
			{3}];
		fieldBitFieldRaw = 
			Map[ If[#1 == {}, Null, #1] &, 
				Map[Cases[#[[3]], XMLElement["BitField", _, _], Infinity] &, 
					characteristicFields,
				{3}],
			{3}];
	   	bitFieldBit = 
			Map[
				If[MatchQ[#, Null],
					Null, 
					Cases[#, XMLElement["Bit", _, _], Infinity]
				] &, 
				fieldBitFieldRaw,
			{3}];
		bitFieldBitSize = 
			Map[
				If[MatchQ[#, Null],
					Null,
					"bitSize" -> "size" /. Part[#, 2] & /@ #
				] &, 
				bitFieldBit,
			{3}];
		bitFieldBitName = 
			Map[
				If[MatchQ[#, Null],
					Null, 
					"bitName" -> "name" /. Part[#, 2] & /@ #
				] &, 
				bitFieldBit,
			{3}];
		bitFieldBitEnumeration = 
			Map[
				If[MatchQ[#, Null],
					Null, 
					Map["bitEnumeration" -> ("key" -> "value" /. Part[#, 2] & /@ #[[3, All]]) &, 
						Cases[#, XMLElement["Enumerations", _, _], 5]
					]
				] &, 
				bitFieldBit,
			{3}];
		bitFieldBitRequires = 
			Map[
				If[MatchQ[#, Null],
					Null, 
					Map["bitRequires" -> (("key" -> "requires" /. Part[#, 2])/.("requires"->Null) & /@ #[[3, All]]) &, 
						Cases[#, XMLElement["Enumerations", _, _], 5]
					]
				] &, 
				bitFieldBit,
			{3}];
		bitFieldBitAttributes = 
			Map["bitAttributes" -> # &, 
				Merge[{bitFieldBitSize, bitFieldBitName, bitFieldBitEnumeration,bitFieldBitRequires}, 4] /.
					List[___?(MatchQ[#, Null] &)] -> Null,
			{3}];
		fieldAttributes = 
			Merge[{fieldNames, fieldRequirements, fieldFormats, fieldUnits, 
				   fieldDecimalExponent, fieldInformativeText, fieldEnumeration, 
					bitFieldBitAttributes}, Infinity];
	   characteristicAttributes = 
			Map["serviceCharacteristics" -> # &, 
				Merge[{characteristicNames, characteristicTypes, 
						characteristicUUIDs, characteristicRequirements, 
						characteristicProperties, 
				Map["characteristicFields" -> # &,
					fieldAttributes,
				{2}]},
			2], 
	     1];
	   	serviceInfo =
			Map[Join[#[[1]], {#[[2]]}] &,
				Merge[{serviceInfo, characteristicAttributes}, 2],
			1];
	   	Return@serviceInfo;
   	];
	
End[]

EndPackage[]

