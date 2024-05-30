import { CurrentTenantDocument } from "@/graphql/Tenant";
import React from "react";
import { useQuery } from "urql";

export function useTenant() {
  const [{ data, fetching }] = useQuery({ query: CurrentTenantDocument, variables: {} });
  const result = React.useMemo(() => ({
    data: data?.tenant,
    fetching,
  }), [data, fetching]);
  return result;
}
