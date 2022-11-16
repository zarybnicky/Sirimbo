import * as React from 'react';
import { Card } from 'components/Card';
import { CallToAction } from 'components/CallToAction';
import { Heading } from 'components/Heading';
import { HtmlView } from 'components/HtmlView';
import { useCohortListQuery } from 'lib/graphql/Cohorts';

export default function CohortsPage() {
  const { data: cohorts } = useCohortListQuery({ visible: true });

  return <>
    <Heading color={{ r: 20, g: 20, b: 200, a: .5 }} text="Tréninkové skupiny" image="" />
    <div className="container mx-auto max-w-3xl mt-8 mb-8">
      {cohorts?.skupinies?.nodes?.map((x, i) => (
        <Card key={i} className="mb-8 flex">
          <div className="-m-3 mr-2 min-w-[2rem] border-r border-slate-200" style={{ backgroundColor: x.sColorRgb }} />
          <div className="">
            <h5>{x.sName}</h5>
            <h6>{x.sLocation}</h6>
            <HtmlView content={x.sDescription.replace('&nbsp;', ' ').replace('<br />', '')} />
          </div>
        </Card>
      ))}
    </div>
    <CallToAction />
  </>;
};
