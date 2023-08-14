import { Resource, CalendarEvent } from "./types"

export default function makeGrouper(resources: Resource[]) {
  return {
    map<T>(fn: (x: [Resource | undefined, number], ix: number) => T): T[] {
      if (!resources || !resources.length) return [fn([undefined, -1], 0)]
      return resources.map((resource, idx) => fn([resource, resource.resourceId], idx))
    },

    groupEvents(events: CalendarEvent[]): Map<number|undefined, CalendarEvent[]> {
      const eventsByResource = new Map()

      if (!resources || !resources.length) {
        // Return all events if resources are not provided
        eventsByResource.set(-1, events)
        return eventsByResource
      }

      events.forEach((event) => {
        (event.resourceIds || [-1]).forEach((id) => {
          const resourceEvents = eventsByResource.get(id) || []
          resourceEvents.push(event)
          eventsByResource.set(id, resourceEvents)
        })
      })
      return eventsByResource
    },
  }
}
