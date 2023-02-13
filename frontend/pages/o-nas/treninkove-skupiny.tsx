import * as React from 'react';
import { Card } from 'components/Card';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { useCohortListQuery } from 'lib/graphql/Cohorts';
import { Layout } from 'components/layout/Layout';
import { RichTextView } from 'components/RichTextView';

export default function CohortsPage() {
  const { data: cohorts } = useCohortListQuery({ visible: true });

  return <>
    <Heading color={{ r: 20, g: 20, b: 200, a: .5 }} text="Tréninkové skupiny" image="" />
    <div className="container mx-auto max-w-3xl mt-8 mb-8">
      {cohorts?.skupinies?.nodes?.map((x, i) => (
        <Card key={i} className="mb-8 flex">
          <div className="-m-3 mr-2 min-w-[2rem] border-r border-stone-200" style={{ backgroundColor: x.sColorRgb }} />
          <div className="">
            <h5 className="text-xl font-bold mb-2">{x.sName}</h5>
            <h6 className="font-bold mb-2">{x.sLocation}</h6>
            <RichTextView value={x.sDescription.replace('&nbsp;', ' ').replace('<br />', '')} />
          </div>
        </Card>
      ))}
    </div>
    <CallToAction />
  </>;
};

CohortsPage.getLayout = (page: React.ReactElement) => <Layout showTopMenu>{page}</Layout>;
