import { Resource, CalendarEvent } from "./types"

export default function makeGrouper(resources: Resource[]) {
  return {
    map<T>(fn: (x: [Resource | undefined, string], ix: number) => T): T[] {
      if (!resources || !resources.length) return [fn([undefined, ''], 0)]
      return resources.map((resource, idx) => fn([resource, resource.resourceId], idx))
    },

    groupEvents(events: CalendarEvent[]): Map<string|undefined, CalendarEvent[]> {
      const eventsByResource = new Map()

      if (!resources || !resources.length) {
        // Return all events if resources are not provided
        eventsByResource.set('', events)
        return eventsByResource
      }

      events.forEach((event) => {
        (event.resourceIds || ['']).forEach((id) => {
          if (!resources.find(x => x.resourceId === id)) return;
          const resourceEvents = eventsByResource.get(id) || []
          resourceEvents.push(event)
          eventsByResource.set(id, resourceEvents)
        })
      })
      return eventsByResource
    },
  }
}
