import { useAuth } from 'lib/data/use-auth';
import { CohortDocument } from 'lib/graphql/Cohorts';
import { CurrentTenantDocument } from 'lib/graphql/Tenant';
import { useGqlQuery } from 'lib/query';
import { Card } from './Card';
import { RichTextView } from './RichTextView';

export function TenantInformation() {
  const { user } = useAuth();
  const { data: cohortData } = useGqlQuery(
    CohortDocument,
    { id: user?.uSkupina! },
    { enabled: !!user?.uSkupina },
  );
  const { data: tenant } = useGqlQuery(CurrentTenantDocument, {});
  const data = tenant?.getCurrentTenant;
  const cohort = cohortData?.skupiny;
  if (!data) {
    return null;
  }

  return (
    <div className="flex flex-col">
      {/* <div className="flex items-center mb-2">&nbsp;</div> */}
      {/* <h3 className="text-3xl tracking-wide mb-4">Můj klub</h3>

      <Card className="w-full px-4">
        <RichTextView value={data.memberInfo} />
      </Card> */}

      {cohort && cohort.sVisible && (
        <>
          <h3 className="text-2xl tracking-wide mb-4">Moje tréninková skupina</h3>
          <Card cohort={cohort}>
            <h3 className="text-2xl tracking-wide mb-4">{cohort.sName}</h3>
            <RichTextView value={cohort.sDescription} />
            <RichTextView value={cohort.internalInfo} />
          </Card>
        </>
      )}

      {/* <h3 className="mt-4 text-2xl tracking-wide">Místa</h3>
        {data.locationsByTenant.nodes.map(item => (
        <div key={item.id}>
        <h4 className="text-xl tracking-wide">{item.name}</h4>
        <RichTextView value={item.description} />
        </div>
        ))} */}
    </div>
  );
}
