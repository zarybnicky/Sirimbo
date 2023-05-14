import * as React from 'react';
import { Card } from 'components/Card';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { useCohortListQuery } from 'lib/graphql/Cohorts';
import { Layout } from 'components/layout/Layout';
import { RichTextView } from 'components/RichTextView';

export default function CohortsPage() {
  const { data: cohorts } = useCohortListQuery({ visible: true });

  return (
    <>
      <Heading>Tréninkové skupiny</Heading>

      <div className="container mx-auto max-w-3xl mt-8 mb-8">
        {cohorts?.skupinies?.nodes?.map((x, i) => (
          <Card key={i} cohort={x}>
            <h5 className="text-xl font-bold">{x.sName}</h5>
            <h6 className="font-bold mb-2">{x.sLocation}</h6>
            <RichTextView
              value={x.sDescription.replace('&nbsp;', ' ').replace('<br />', '')}
            />
          </Card>
        ))}
      </div>
      <CallToAction />
    </>
  );
}

CohortsPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
