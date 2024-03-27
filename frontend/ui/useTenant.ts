import { CurrentTenantDocument, CurrentTenantQueryVariables } from "@/graphql/Tenant";
import React from "react";
import { useQuery } from "urql";

export function useTenant(variables: CurrentTenantQueryVariables = {}) {
  const [{ data, fetching }] = useQuery({ query: CurrentTenantDocument, variables });
  const result = React.useMemo(() => ({
    data: data?.tenant,
    fetching,
  }), [data, fetching]);
  return result;
}
