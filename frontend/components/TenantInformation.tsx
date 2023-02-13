import { useAuth } from "lib/data/use-auth";
import { useCohortQuery } from "lib/graphql/Cohorts";
import { useCurrentTenantQuery } from "lib/graphql/Tenant";
import { RichTextView } from "./RichTextView";

export function TenantInformation() {
  const { user } = useAuth();
  const { data: cohortData } = useCohortQuery({ id: user?.uSkupina! });
  const { data: tenant } = useCurrentTenantQuery();
  const data = tenant?.getCurrentTenant;
  const cohort = cohortData?.skupiny;
  if (!data) {
    return null;
  }

  return <div className="flex flex-col items-center">
    <h3 className="text-2xl tracking-wide">{data.name}</h3>

    <div className="w-full px-4">
      <RichTextView value={data.memberInfo} />
    </div>

    {cohort && cohort.sVisible && <>
      <h3 className="text-2xl tracking-wide">{cohort.sName}</h3>
      <div className="w-full px-4">
        <RichTextView value={cohort.sDescription} />
        <RichTextView value={cohort.internalInfo} />
      </div>
    </>}

    {/* <h3 className="mt-4 text-2xl tracking-wide">Místa</h3>
        {data.locationsByTenant.nodes.map(item => (
        <div key={item.id}>
        <h4 className="text-xl tracking-wide">{item.name}</h4>
        <RichTextView value={item.description} />
        </div>
        ))} */}
  </div>;
}
