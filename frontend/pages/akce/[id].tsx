import { EventSeriesDocument } from '@/graphql/Event';
import { Layout } from '@/ui/Layout';
import { Spinner } from '@/ui/Spinner';
import { FormError } from '@/ui/form';
import { useTypedRouter, zRouterId } from '@/ui/useTypedRouter';
import { useRouter } from 'next/router';
import * as React from 'react';
import { useQuery } from 'urql';
import { z } from 'zod';

const QueryParams = z.object({
  id: zRouterId,
});

export default function LegacyEventRedirectPage() {
  const router = useRouter();
  const {
    query: { id },
  } = useTypedRouter(QueryParams);
  const [{ data, error, fetching }] = useQuery({
    query: EventSeriesDocument,
    variables: { id },
    pause: !id,
  });
  const series = data?.eventSeries;

  React.useEffect(() => {
    if (!series) return;
    const destination =
      series.eventsList.length === 1
        ? `/termin/${series.eventsList[0]!.id}`
        : `/terminy/${series.id}`;
    void router.replace(destination);
  }, [router, series]);

  return (
    <Layout hideTopMenuIfLoggedIn>
      <div className="col-feature min-h-[60vh] p-4 lg:pb-8">
        <FormError error={error} />
        {fetching || series ? <Spinner /> : null}
        {!fetching && !series ? <p>Událost nebyla nalezena.</p> : null}
      </div>
    </Layout>
  );
}
