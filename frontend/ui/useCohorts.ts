import { CohortListDocument, CohortListQueryVariables } from "@/graphql/Cohorts";
import React from "react";
import { useQuery } from "urql";

export function useCohorts(variables: CohortListQueryVariables = {}) {
  const [{ data, fetching }] = useQuery({ query: CohortListDocument, variables });
  const result = React.useMemo(() => ({
    data: data?.getCurrentTenant?.cohortsList || [],
    fetching,
  }), [data, fetching]);
  return result;
}
