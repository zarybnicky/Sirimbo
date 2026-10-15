'use client';

import React from 'react';
import Link from 'next/link';
import * as Plot from '@observablehq/plot';
import type { ExtendedFeatureCollection } from 'd3';
import { parseAsInteger, useQueryState } from 'nuqs';
import { useQuery } from 'urql';
import {
  FederationEventFacetsDocument,
  FederationMapEventsDocument,
  type FederationMapEventFragment,
} from '@/graphql/FederationMap';
import { SelectField } from '@/ui/fields/select';
import { Dialog, DialogContent, DialogTitle, DialogTrigger } from '@/ui/dialog';
import { Table } from '@/ui/Table';
import { FormError } from '@/ui/form';
import { formatDateOnly } from '@/ui/format';

export default function EventMap({ federation }: { federation: string }) {
  const [year, setYear] = useQueryState(
    'year',
    parseAsInteger
      .withDefault(new Date().getFullYear())
      .withOptions({ history: 'push', scroll: false }),
  );
  const [facets] = useQuery({
    query: FederationEventFacetsDocument,
    variables: { federation },
  });
  const years = (facets.data?.federation?.eventFacets?.years ?? []).filter(
    (y) => y !== null,
  );
  const [result] = useQuery({
    query: FederationMapEventsDocument,
    variables: {
      federation,
      date: { since: `${year}-01-01`, until: `${year + 1}-01-01` },
    },
  });
  const events = result.data?.federation?.events.nodes ?? [];
  const missing = events.filter((e) => e.venueLat === null || e.venueLng === null);
  const error = facets.error || result.error;
  if (error) return <FormError error={error} />;

  return (
    <div className="min-w-0">
      <div className="mb-4 flex flex-wrap items-end gap-3">
        <SelectField
          name="year"
          label="Rok"
          value={String(year)}
          onChange={(value) => void setYear(Number(value))}
          disabled={years.length === 0}
          options={years.map((y) => ({
            value: String(y.year),
            label: `${y.year} · ${y.eventCount} událostí (${y.mappedEventCount} na mapě)`,
          }))}
        />
        {missing.length > 0 && (
          <Dialog key={year}>
            <DialogTrigger size="sm" text={`Bez souřadnic (${missing.length})`} />
            <DialogContent className="sm:max-w-3xl">
              <DialogTitle>Events without coordinates · {year}</DialogTitle>
              <Table columns={['Date', 'Event', 'Location', 'Country']}>
                {missing.map((event) => (
                  <tr key={event.id} className="border-t border-neutral-4">
                    <td className="whitespace-nowrap">
                      {formatDateOnly(event.startDate)}
                    </td>
                    <th scope="row">
                      <Link
                        className="text-accent-11 underline"
                        href={`/federated/${federation}/${event.externalId}`}
                      >
                        {event.name || event.externalId}
                      </Link>
                    </th>
                    <td>
                      <div>{event.city || event.location || '–'}</div>
                      <div>
                        {[event.streetAddress, event.postalCode]
                          .filter(Boolean)
                          .join(', ')}
                      </div>
                    </td>
                    <td>{event.country || '–'}</td>
                  </tr>
                ))}
              </Table>
            </DialogContent>
          </Dialog>
        )}
      </div>
      <MapPlot events={events} />
    </div>
  );
}

function MapPlot({ events }: { events: FederationMapEventFragment[] }) {
  const [cz, setCz] = React.useState<ExtendedFeatureCollection | null>(null);
  const containerRef = React.useRef<HTMLDivElement | null>(null);
  React.useEffect(() => {
    fetch('/cz-admin1.json')
      .then((x) => x.json())
      .then(setCz);
  }, []);
  React.useEffect(() => {
    if (!cz || !containerRef.current) return;
    const months = [
      'leden',
      'únor',
      'březen',
      'duben',
      'květen',
      'červen',
      'červenec',
      'srpen',
      'září',
      'říjen',
      'listopad',
      'prosinec',
    ];
    const plot = Plot.plot({
      projection: {
        type: 'azimuthal-equidistant',
        domain: cz,
        inset: 5,
        rotate: [-20, 0],
      },
      axis: null,
      color: { type: 'categorical', scheme: 'sinebow', domain: months, legend: true },
      style: { fontFamily: 'Georgia' },
      marks: [
        Plot.geo(cz, { stroke: '#aaa' }),
        Plot.dot(
          events.filter((e) => e.venueLat !== null && e.venueLng !== null),
          {
            y: 'venueLat',
            x: 'venueLng',
            fill: (x) => months[Number(x.startDate.slice(5, 7)) - 1],
            r: 2,
            channels: {
              title: (x) => [formatDateOnly(x.startDate), x.name, x.location].join('\n'),
            },
            tip: true,
          },
        ),
      ],
    });
    containerRef.current.append(plot);
    return () => plot.remove();
  }, [cz, events]);
  return <div ref={containerRef} />;
}
