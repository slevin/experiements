using UnityEngine;
using System.Collections;
using System.Collections.Generic;

public class Sphere : MonoBehaviour {

	public float size;
	public Bounds bounds;

	private Vector3 velocity;

	// Use this for initialization
	void Start () {
	
	}

	public void ApplyForces(List<Vector3> forces) {
		foreach (Vector3 f in forces) {
			this.velocity = this.velocity + f;
		}
		transform.position = transform.position + (velocity * Time.deltaTime / size);

		if ((transform.position.x < this.bounds.minX && this.velocity.x < 0) ||
		    (transform.position.x > this.bounds.maxX && this.velocity.x > 0)) {
			velocity = new Vector3(-this.velocity.x, this.velocity.y, this.velocity.z);
			transform.position = new Vector3(Mathf.Clamp(transform.position.x, bounds.minX, bounds.maxX),
			                                 transform.position.y,
			                                 transform.position.z);
		}
		if ((transform.position.z < this.bounds.minZ && this.velocity.z < 0) ||
			(transform.position.z > this.bounds.maxZ && this.velocity.z > 0)) {
			velocity = new Vector3(this.velocity.x, this.velocity.y, -this.velocity.z);
			transform.position = new Vector3(transform.position.x,
			                                   transform.position.y,
			                                   Mathf.Clamp(transform.position.z, bounds.minZ, bounds.maxZ));
		}	
	}

	// Update is called once per frame
	void Update () {
		 
	}
}
