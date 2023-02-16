package smithy4s.compliancetests

object AwsTestSupportedTestIds {

  val testIds = Set(
    "RestJsonEmptyInputAndEmptyOutput",
    "RestJsonNoInputAndNoOutput",
    "RestJsonUnitInputAndOutputNoOutput",
    "RestJsonNoInputAndOutputWithJson",
    "RestJsonEmptyInputAndEmptyOutputJsonObjectOutput",
    "RestJsonNoInputAndOutputNoPayload",
    "RestJsonNoInputAndNoOutput",
    "RestJsonNoInputAndOutput",
    "RestJsonUnitInputAndOutput",
    "RestJsonEmptyInputAndEmptyOutput",
    "RestJsonTimestampFormatHeaders",
    "RestJsonHttpRequestWithGreedyLabelInPath",
    "RestJsonToleratesRegexCharsInSegments",
    "RestJsonQueryStringEscaping",
    "RestJsonQueryStringMap",
    "RestJsonConstantAndVariableQueryStringMissingOneValue",
    "RestJsonConstantQueryString",
    "RestJsonConstantAndVariableQueryStringAllValues",
    "RestJsonOmitsNullQuery",
    "RestJsonIgnoreQueryParamsInResponse",
    "RestJsonSerializesEmptyQueryValue",
    "RestJsonQueryParamsStringListMap",
    "RestJsonHttpPrefixHeadersArePresent",
    "RestJsonHttpPrefixHeadersAreNotPresent",
    "RestJsonHttpPrefixHeadersArePresent",
    "RestJsonHttpPayloadTraitsWithNoBlobBody",
    "RestJsonHttpPayloadWithStructure",
    "RestJsonJsonTimestampsWithEpochSecondsOnTargetFormat",
    "RestJsonJsonTimestampsWithHttpDateFormat",
    "RestJsonJsonTimestampsWithHttpDateOnTargetFormat",
    "RestJsonJsonTimestampsWithEpochSecondsFormat",
    "RestJsonJsonTimestampsWithDateTimeOnTargetFormat",
    "RestJsonJsonTimestampsWithDateTimeFormat",
    "RestJsonJsonTimestampsWithEpochSecondsOnTargetFormat",
    "RestJsonJsonTimestampsWithHttpDateFormat",
    "RestJsonJsonTimestampsWithHttpDateOnTargetFormat",
    "RestJsonRecursiveShapes",
    "RestJsonRecursiveShapes",
    "RestJsonJsonIntEnums",
    "RestJsonJsonEnums",
    "RestJsonLists",
    "RestJsonJsonEnums",
    "RestJsonJsonIntEnums",
    "RestJsonListsEmpty",
    "RestJsonSerializesSparseSetMap",
    "RestJsonJsonMaps",
    "RestJsonSerializesDenseSetMap",
    "RestJsonSerializesZeroValuesInMaps",
    "RestJsonListsEmpty",
    "RestJsonLists",
    "RestJsonJsonMaps",
    "RestJsonDeserializesZeroValuesInMaps",
    "RestJsonDeserializesSparseSetMap",
    "DocumentInputWithString",
    "DocumentTypeInputWithObject",
    "RestJsonJsonBlobs",
    "DocumentInputWithNumber",
    "DocumentOutputNumber",
    "RestJsonDeserializesDenseSetMap",
    "DocumentOutputString",
    "DocumentInputWithList",
    "DocumentOutputArray",
    "DocumentOutput",
    "DocumentInputWithBoolean",
    "RestJsonJsonBlobs",
    "DocumentOutputBoolean",
    "DocumentTypeAsPayloadOutputString",
    "DocumentTypeAsPayloadInputString",
    "DocumentTypeAsPayloadInput",
    "DocumentTypeAsPayloadOutput",
    "RestJsonSerializeBooleanUnionValue",
    "RestJsonSerializeStringUnionValue",
    "RestJsonSerializeNumberUnionValue",
    "RestJsonSerializeListUnionValue",
    "RestJsonSerializeEnumUnionValue",
    "RestJsonSerializeStructureUnionValue",
    "RestJsonSerializeMapUnionValue",
    "RestJsonDeserializeStringUnionValue",
    "RestJsonDeserializeBooleanUnionValue",
    "RestJsonDeserializeNumberUnionValue",
    "RestJsonDeserializeMapUnionValue",
    "RestJsonDeserializeListUnionValue",
    "RestJsonDeserializeEnumUnionValue",
    "RestJsonInputUnionWithUnitMember",
    "PostUnionWithJsonNameRequest1",
    "RestJsonOutputUnionWithUnitMember",
    "RestJsonDeserializeStructureUnionValue",
    "PostUnionWithJsonNameRequest2",
    "PostUnionWithJsonNameRequest3",
    "RestJsonEndpointTrait",
    "PostUnionWithJsonNameResponse1",
    "RestJsonEndpointTraitWithHostLabel",
    "PostUnionWithJsonNameResponse3",
    "PostUnionWithJsonNameResponse2",
    "RestJsonTestBodyStructure",
    "RestJsonHttpWithEmptyBody",
    "RestJsonHttpWithHeadersButNoPayload",
    "RestJsonHttpWithEmptyStructurePayload",
    "RestJsonTestPayloadStructure",
    "RestJsonNoInputAndNoOutput",
    "RestJsonUnitInputAndOutputNoOutput",
    "RestJsonNoInputAllowsAccept",
    "RestJsonNoInputAndOutput",
    "RestJsonNoInputAndOutputAllowsAccept",
    "RestJsonNoInputAndNoOutput",
    "RestJsonEmptyInputAndEmptyOutput",
    "RestJsonEmptyInputAndEmptyOutputWithJson",
    "RestJsonTimestampFormatHeaders",
    "RestJsonUnitInputAndOutput",
    "RestJsonUnitInputAllowsAccept",
    "RestJsonInputAndOutputWithStringHeaders",
    "RestJsonToleratesRegexCharsInSegments",
    "RestJsonHttpRequestWithGreedyLabelInPath",
    "RestJsonInputWithHeadersAndAllParams",
    "RestJsonHttpRequestLabelEscaping",
    "RestJsonHttpRequestWithLabelsAndTimestampFormat",
    "RestJsonQueryStringEscaping",
    "RestJsonQueryStringMap",
    "RestJsonIgnoreQueryParamsInResponse",
    "RestJsonConstantQueryString",
    "RestJsonConstantAndVariableQueryStringAllValues",
    "RestJsonSerializesEmptyQueryValue",
    "RestJsonConstantAndVariableQueryStringMissingOneValue",
    "RestJsonHttpPrefixHeadersArePresent",
    "RestJsonHttpPrefixHeadersAreNotPresent",
    "RestJsonHttpPrefixHeadersArePresent",
    "RestJsonServersAcceptStaticQueryParamAsEmptyString",
    "RestJsonHttpPayloadWithStructure",
    "RestJsonHttpPayloadTraitsWithNoBlobBody",
    "RestJsonHttpPayloadWithStructure",
    "RestJsonJsonTimestampsWithDateTimeFormat",
    "RestJsonJsonTimestampsWithDateTimeFormat",
    "RestJsonJsonTimestampsWithDateTimeOnTargetFormat",
    "RestJsonJsonTimestampsWithDateTimeOnTargetFormat",
    "RestJsonJsonTimestampsWithEpochSecondsFormat",
    "RestJsonJsonTimestampsWithHttpDateOnTargetFormat",
    "RestJsonJsonTimestampsWithHttpDateFormat",
    "RestJsonJsonTimestampsWithHttpDateOnTargetFormat",
    "RestJsonJsonTimestampsWithHttpDateFormat",
    "RestJsonJsonTimestampsWithEpochSecondsFormat",
    "RestJsonJsonTimestampsWithEpochSecondsOnTargetFormat",
    "RestJsonJsonIntEnums",
    "RestJsonJsonTimestampsWithEpochSecondsOnTargetFormat",
    "RestJsonJsonIntEnums",
    "RestJsonJsonEnums",
    "RestJsonJsonEnums",
    "RestJsonRecursiveShapes",
    "RestJsonListsEmpty",
    "RestJsonRecursiveShapes",
    "RestJsonLists",
    "RestJsonListsEmpty",
    "RestJsonLists",
    "RestJsonJsonMaps",
    "RestJsonJsonMaps",
    "RestJsonDeserializesZeroValuesInMaps",
    "RestJsonDeserializesSparseSetMap",
    "RestJsonJsonBlobs",
    "RestJsonDeserializesDenseSetMap",
    "RestJsonSerializesSparseSetMap",
    "RestJsonJsonBlobs",
    "RestJsonSerializesZeroValuesInMaps",
    "RestJsonSerializesDenseSetMap",
    "DocumentTypeInputWithObject",
    "DocumentOutputString",
    "DocumentOutput",
    "DocumentOutputArray",
    "DocumentOutputBoolean",
    "DocumentOutputNumber",
    "DocumentTypeAsPayloadOutput",
    "DocumentTypeAsPayloadOutputString",
    "DocumentTypeAsPayloadInput",
    "DocumentTypeAsPayloadInputString",
    "DocumentInputWithString",
    "DocumentInputWithNumber",
    "DocumentInputWithBoolean",
    "DocumentInputWithList",
    "RestJsonSerializeBooleanUnionValue",
    "RestJsonSerializeNumberUnionValue",
    "RestJsonSerializeStringUnionValue",
    "RestJsonDeserializeStringUnionValue",
    "RestJsonDeserializeNumberUnionValue",
    "RestJsonDeserializeListUnionValue",
    "RestJsonDeserializeEnumUnionValue",
    "RestJsonDeserializeBooleanUnionValue",
    "RestJsonDeserializeMapUnionValue",
    "RestJsonDeserializeStructureUnionValue",
    "RestJsonSerializeMapUnionValue",
    "RestJsonOutputUnionWithUnitMember",
    "PostUnionWithJsonNameResponse2",
    "PostUnionWithJsonNameResponse1",
    "RestJsonSerializeStructureUnionValue",
    "RestJsonSerializeListUnionValue",
    "PostUnionWithJsonNameResponse3",
    "RestJsonTestBodyStructure",
    "RestJsonEndpointTrait",
    "RestJsonSerializeEnumUnionValue",
    "RestJsonInputUnionWithUnitMember",
    "PostUnionWithJsonNameRequest2",
    "PostUnionWithJsonNameRequest3",
    "PostUnionWithJsonNameRequest1",
    "RestJsonHttpWithEmptyStructurePayload",
    "RestJsonHttpWithEmptyBody",
    "RestJsonEndpointTraitWithHostLabel",
    "RestJsonTestPayloadStructure",
    "RestJsonHttpWithHeadersButNoPayload",
    "RestJsonHttpWithNoModeledBody",
    "RestJsonHttpWithHeaderMemberNoModeledBody",
    "RestJsonHttpResponseCodeWithNoPayload",
    "RestJsonHttpResponseCode",
    "RestJsonJsonTimestampsWithDateTimeFormat",
    "RestJsonJsonTimestampsWithDateTimeOnTargetFormat",
    "RestJsonHttpPayloadWithStructure"
  )

}
