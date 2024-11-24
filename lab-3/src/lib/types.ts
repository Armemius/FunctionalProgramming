export interface Point {
    x: number,
    y: number,
}

export interface InterpolationResult {
    method: string,
    latex_equation: string,
    points: Point[]
}

export interface InterpolationType {
    id: string,
    name: string,
    displayName: string,
    color?: string,
}