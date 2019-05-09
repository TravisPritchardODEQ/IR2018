SELECT OrganizationID, [MLocID], [SampleStartDate],[SampleStartTime],[Char_Name],[IRResultNWQSunit], count(*) as count, Activity_Type
FROM [IntegratedReport].[dbo].[ResultsRawWater2018]
WHERE ((Statistical_Base = 'Minimum')  AND Char_Name = 'Dissolved oxygen saturation') OR 
      ((Statistical_Base IS NULL)  AND Char_Name = 'Dissolved oxygen saturation')
group by OrganizationID, [MLocID], [SampleStartDate],[SampleStartTime],[Statistical_Base], [IRResultNWQSunit], [Char_Name], Activity_Type
Having count(*) > 1
order by OrganizationID, MLocID, SampleStartDate, SampleStartTime