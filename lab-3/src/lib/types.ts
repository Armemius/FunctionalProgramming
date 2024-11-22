export interface Point {
    x: number,
    y: number,
}

export interface InterpolationResult {
    points: Point[],
}

export interface InterpolationType {
    id: string,
    name: string,
    color?: string,
}