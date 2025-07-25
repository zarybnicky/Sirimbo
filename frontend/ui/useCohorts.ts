import { CohortListDocument, type CohortListQueryVariables } from "@/graphql/Cohorts";
import React from "react";
import { useQuery } from "urql";

export function useCohorts(variables: CohortListQueryVariables = {}) {
  const [{ data, fetching }] = useQuery({ query: CohortListDocument, variables });
  return React.useMemo(() => ({
    data: data?.getCurrentTenant?.cohortsList || [],
    fetching,
  }), [data, fetching]);
}
